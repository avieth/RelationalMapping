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
import Data.Relational
import Data.Relational.RelationalMapping
import Data.Relational.Universe
import Data.Relational.Interpreter
import Data.Relational.PostgreSQL
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import Database.PostgreSQL.Simple.ToField

newtype User = User T.Text
  deriving (Show, Eq)

newtype MessageFrom = MessageFrom User
  deriving (Show, Eq)

newtype MessageTo = MessageTo User
  deriving (Show, Eq)

newtype MessageBody = MessageBody T.Text
  deriving (Show)

newtype MessageViewed = MessageViewed Bool
  deriving (Show)

newtype MessageSendTime = MessageSendTime UTCTime
  deriving (Eq)

instance Show (MessageSendTime) where
  show (MessageSendTime utctime) = show (utcToLocalTime utc utctime)

data Message = Message MessageFrom MessageTo MessageSendTime MessageViewed MessageBody
  deriving Show

-- This is a reasonable Eq instance, as surely no user can send more than one
-- message to the same user at any particular instant.
instance Eq Message where
  (Message mfrom mto msendtime _ _) == (Message mfrom' mto' msendtime' _ _) =
      msendtime == msendtime' && mfrom == mfrom' && mto == mto'

type MessageFromColumn = '("user_from", MessageFrom)
type MessageToColumn = '("user_to", MessageTo)
type MessageSendTimeColumn = '("sent", MessageSendTime)
type MessageViewedColumn = '("read", MessageViewed)
type MessageBodyColumn = '("message", MessageBody)
type MessageSchema = '[ MessageFromColumn, MessageToColumn, MessageSendTimeColumn, MessageViewedColumn, MessageBodyColumn ]

messageFromColumn :: Column MessageFromColumn
messageFromColumn = column

messageToColumn :: Column MessageToColumn
messageToColumn = column

messageSendTimeColumn :: Column MessageSendTimeColumn
messageSendTimeColumn = column

messageViewedColumn :: Column MessageViewedColumn
messageViewedColumn = column

messageBodyColumn :: Column MessageBodyColumn
messageBodyColumn = column

instance RelationalMapping Message where

  type RelationalTableName Message = "messages"
  type RelationalSchema Message = MessageSchema

  relationalSchema proxy =
         messageFromColumn
      :| messageToColumn
      :| messageSendTimeColumn
      :| messageViewedColumn
      :| messageBodyColumn
      :| EndSchema

  rowBijection = Bi toRow fromRow

    where

      toRow :: Message -> Row MessageSchema
      toRow (Message mfrom mto msendtime mviewed mbody) =
              (Field Proxy mfrom)
          :&| (Field Proxy mto)
          :&| (Field Proxy msendtime)
          :&| (Field Proxy mviewed)
          :&| (Field Proxy mbody)
          :&| EndRow

      fromRow :: Row MessageSchema -> Message
      fromRow (mfrom :&| mto :&| msendtime :&| mviewed :&| mbody :&| EndRow) =
          Message
            (fieldValue mfrom)
            (fieldValue mto)
            (fieldValue msendtime)
            (fieldValue mviewed)
            (fieldValue mbody)

fromRow :: RelationalMapping d => Row (RelationalSchema d) -> d
fromRow = biFrom rowBijection

toRow :: RelationalMapping d => d -> Row (RelationalSchema d)
toRow = biTo rowBijection

newMessage :: MessageFrom -> MessageTo -> MessageBody -> IO Message
newMessage mfrom mto mbody = do
    t <- getCurrentTime
    let msendtime = MessageSendTime t
    let mviewed = MessageViewed False
    return $ Message mfrom mto msendtime mviewed mbody

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

instance InUniverse PostgresUniverse MessageViewed where
  toUniverse proxy (MessageViewed b) = UBool b
  fromUniverse proxy (UBool b) = Just (MessageViewed b)
  type UniverseType PostgresUniverse MessageViewed = Bool
  toUniverseAssociated proxy = UBool
  fromUniverseAssociated (UBool b) = b

instance InUniverse PostgresUniverse MessageSendTime where
  toUniverse proxy (MessageSendTime t) = UUTCTime t
  fromUniverse proxy (UUTCTime t) = Just (MessageSendTime t)
  type UniverseType PostgresUniverse MessageSendTime = UTCTime
  toUniverseAssociated proxy = UUTCTime
  fromUniverseAssociated (UUTCTime t) = t

postgresProxy :: Proxy PostgresInterpreter
postgresProxy = Proxy

insertMessage :: Message -> PostgresMonad ()
insertMessage message = insert message postgresProxy

markAsRead :: Message -> Message
markAsRead (Message mto mfrom msent mviewed mbody) = Message mto mfrom msent (MessageViewed True) mbody

readMessages
  :: forall conditioned .
     ( SelectConstraint Message PostgresInterpreter conditioned
     , Monad PostgresMonad
     )
  => Condition conditioned
  -> PostgresMonad [Maybe Message]
readMessages condition = do
    rows :: [Maybe Message] <- select (Proxy :: Proxy Message) postgresProxy condition
    let rowsRead = (fmap . fmap) markAsRead rows
    let rowsUpdate = (fmap . fmap) (\x -> update postgresProxy x (mkUpdateCondition x)) rowsRead
    forM rowsUpdate maybeM
    return rows
  where
    mkUpdateCondition (Message mfrom mto msendtime _ _) =
             messageFromColumn .==. mfrom .||. false
        .&&. messageToColumn .==. mto .||. false
        .&&. messageSendTimeColumn .==. msendtime .||. false
        .&&. true

maybeM :: Monad m => Maybe (m a) -> m ()
maybeM mx = case mx of
    Just term -> term >> return ()
    Nothing -> return ()
