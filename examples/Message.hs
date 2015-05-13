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

import Data.Bijection
import Data.Proxy
import Data.Relational
import Data.Relational.RelationalMapping
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime

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

type MessageFromColumn = '("from", MessageFrom)
type MessageToColumn = '("to", MessageTo)
type MessageSendTimeColumn = '("send_time", MessageSendTime)
type MessageViewedColumn = '("viewed", MessageViewed)
type MessageBodyColumn = '("body", MessageBody)
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

  -- There is no sensible Eq instance for Message; it's perfectly reasonable
  -- to find the same message body sent from the same user to the same user
  -- more than once. Our complete characterization uses all fields, and may
  -- pick out more than one Message!
  --
  -- Does it ever make sense to not be able to isolate one thing?
  -- It would be weird, if deleting the haskell datatype deleted more than one
  -- row in its table.
  type CompleteCharacterization Message = '[
        '[MessageFromColumn]
      , '[MessageToColumn]
      , '[MessageSendTimeColumn]
      ]

  completeCharacterization (Message mfrom mto msendtime _ _) =
           messageFromColumn .==. mfrom .||. false 
      .&&. messageToColumn .==. mto .||. false
      .&&. messageSendTimeColumn .==. msendtime .||. false
      .&&. true

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

newMessage :: MessageFrom -> MessageTo -> MessageBody -> IO Message
newMessage mfrom mto mbody = do
    t <- getCurrentTime
    let msendtime = MessageSendTime t
    let mviewed = MessageViewed False
    return $ Message mfrom mto msendtime mviewed mbody
