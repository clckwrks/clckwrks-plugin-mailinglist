{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Clckwrks.MailingList.Types where

import Clckwrks
import Data.Data     (Data, Typeable)
import Data.IxSet    (Indexable(..), ixSet, ixFun)
import Data.Maybe    (maybeToList)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text     (Text)
import Data.Set      (Set)
import Data.Time     (UTCTime)
import Web.Routes    (PathInfo(..))

newtype Email = Email { unEmail :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

newtype SubscriberId = SubscriberId { unSubscriberId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

incSubscriberId :: SubscriberId -> SubscriberId
incSubscriberId (SubscriberId i) = SubscriberId (i + 1)

data Subscriber = Subscriber
   { subId    :: SubscriberId
   , subEmail :: Email
   , subDate  :: UTCTime
   }
   deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Subscriber)

instance Indexable Subscriber where
    empty = ixSet [ ixFun $ (:[]) . subId
                  , ixFun $ (:[]) . subEmail
                  ]

newtype MessageId = MessageId { unMessageId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo, SafeCopy)

data Message = Message
    { msgId      :: MessageId
    , msgSubject :: Text
    , msgFrom    :: Email
    , msgBody    :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Message)

data MessageStatus
    = Draft
    | Sent UTCTime
    | Scheduled UTCTime
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''MessageStatus)

data Envelope = Envelope
    { envStatus :: MessageStatus
    , envMsg    :: Message
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Envelope)
