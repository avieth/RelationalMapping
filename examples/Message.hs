{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Monad
import Data.Bijection
import Data.Proxy
import Data.TypeNat.Nat
import Data.Versioned
import Data.Migration
import Data.Relational
import Data.Relational.RelationalMapping
import Data.Relational.RelationalMappingVersioned
import Data.Relational.Universe
import Data.Relational.Interpreter
import Data.Relational.PostgreSQL
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import Database.PostgreSQL.Simple.ToField

-- We begin by defining the types we wish to work with in business logic, and
-- some functions on them.

newtype User = User T.Text
  deriving (Show, Eq)

username :: User -> T.Text
username (User t) = t

newtype MessageFrom = MessageFrom User
  deriving (Show, Eq)

newtype MessageTo = MessageTo User
  deriving (Show, Eq)

newtype MessageBody = MessageBody T.Text
  deriving (Show)

data Message = Message MessageFrom MessageTo MessageBody
  deriving Show

replyToMessage :: MessageBody -> Message -> Message
replyToMessage mbody (Message (MessageFrom ufrom) (MessageTo uto) _) =
    Message (MessageFrom uto) (MessageTo ufrom) mbody

printMessage :: Message -> T.Text
printMessage (Message (MessageFrom ufrom) (MessageTo uto) (MessageBody mbody)) =
    T.concat
    [ "From: ", username ufrom, "\n"
    , "To: ", username uto, "\n"
    , mbody, "\n"
    , "EOM\n"
    ]

-- Now we endow Message with a versioned history, which is super easy since
-- there's currently just one version.

instance Versioned Message where

  type LatestVersion Message = Zero

  data VersionHistory Message n where

    MessageV0
      :: MessageFrom
      -> MessageTo
      -> MessageBody
      -> VersionHistory Message Zero

  bijectionLatestVersion = Bi toLatest fromLatest
    where
      toLatest :: Message -> VersionHistory Message (LatestVersion Message)
      toLatest (Message mfrom mto mbody) =
          MessageV0 mfrom mto mbody
      fromLatest :: VersionHistory Message (LatestVersion Message) -> Message
      fromLatest (MessageV0 mfrom mto mbody) =
          Message mfrom mto mbody

  migrationPath = TrivialMigrationPath

-- And now we define a relational mapping for the version history of Message.

type MessageFromColumn = '("user_from", MessageFrom)
type MessageToColumn = '("user_to", MessageTo)
type MessageBodyColumn = '("message", MessageBody)
type MessageSchema =
    '[ MessageFromColumn
     , MessageToColumn
     , MessageBodyColumn
     ]

messageFromColumn :: Column MessageFromColumn
messageFromColumn = column

messageToColumn :: Column MessageToColumn
messageToColumn = column

messageBodyColumn :: Column MessageBodyColumn
messageBodyColumn = column

instance RelationalMapping (VersionHistory Message Zero) where

  type RelationalTableName (VersionHistory Message Zero) = "messages_v0"
  type RelationalSchema (VersionHistory Message Zero) = MessageSchema

  relationalSchema proxy =
         messageFromColumn
      :| messageToColumn
      :| messageBodyColumn
      :| EndSchema

  rowBijection = Bi toRow fromRow

    where

      toRow :: VersionHistory Message Zero -> Row MessageSchema
      toRow (MessageV0 mfrom mto mbody) =
              (Field Proxy mfrom)
          :&| (Field Proxy mto)
          :&| (Field Proxy mbody)
          :&| EndRow

      fromRow :: Row MessageSchema -> VersionHistory Message Zero
      fromRow (mfrom :&| mto :&| mbody :&| EndRow) =
          MessageV0
            (fieldValue mfrom)
            (fieldValue mto)
            (fieldValue mbody)

instance InUniverse PostgresUniverse MessageFrom where
  toUniverse proxy (MessageFrom (User t)) = UText t
  fromUniverse proxy (UText t) = Just (MessageFrom (User t))
  type UniverseType PostgresUniverse MessageFrom = T.Text
  toUniverseAssociated proxy = UText
  fromUniverseAssociated (UText t) = t

instance InUniverse PostgresUniverse MessageTo where
  toUniverse proxy (MessageTo (User t)) = UText t
  fromUniverse proxy (UText t) = Just (MessageTo (User t))
  type UniverseType PostgresUniverse MessageTo = T.Text
  toUniverseAssociated proxy = UText
  fromUniverseAssociated (UText t) = t

instance InUniverse PostgresUniverse MessageBody where
  toUniverse proxy (MessageBody t) = UText t
  fromUniverse proxy (UText t) = Just (MessageBody t)
  type UniverseType PostgresUniverse MessageBody = T.Text
  toUniverseAssociated proxy = UText
  fromUniverseAssociated (UText t) = t

postgresProxy :: Proxy PostgresInterpreter
postgresProxy = Proxy

insertMessage :: Message -> PostgresMonad ()
insertMessage message = vinsert postgresProxy message

messagesTo :: MessageTo -> PostgresMonad [Maybe Message]
messagesTo receiver =
    (fmap . fmap . fmap)
    fromLatestVersionHistory
    (vselect postgresProxy (Proxy :: Proxy Message) (Proxy :: Proxy (LatestVersion Message)) condition)
  where
    condition = messageToColumn .==. receiver .||. false .&&. true

messagesFrom :: MessageFrom -> PostgresMonad [Maybe Message]
messagesFrom sender =
    (fmap . fmap . fmap)
    fromLatestVersionHistory
    (vselect postgresProxy (Proxy :: Proxy Message) (Proxy :: Proxy (LatestVersion Message)) condition)
  where
    condition = messageFromColumn .==. sender .||. false .&&. true
