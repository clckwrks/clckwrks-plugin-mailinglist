{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Clckwrks.MailingList.Types where

import Clckwrks
import Control.Lens    ((^.), to)
import Control.Lens.TH (makeLenses)
import Data.Data     (Data, Typeable)
import Data.IxSet    (Indexable(..), ixSet, ixFun)
import Data.Maybe    (maybeToList)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text     (Text, unpack)
import Data.Text.Lazy (toStrict)
import Data.Set      (Set)
import Data.Time     (UTCTime)
import Data.UUID     (UUID)
import Data.UUID.Orphans ()
import Network.Mail.Mime (Address(..), Mail(..), simpleMail')
import Text.StringTemplate (newSTMP, render, setManyAttrib)
import Web.Routes    (PathInfo(..))

-- | Email
--
-- Given the many email address RFCs, validation schemes, etc, it
-- seems best to keep this type simple so that we do not have to
-- do migration later. This should allows us to get a Unicode string
-- from the user in a form and then use some tool to validate it.
--
-- Normalization is also non-standard. For example, most systems are
-- not case sensitive, but some are. Additionally, in gmail . is ignored
-- in the local part.
newtype Email = Email { _unEmail :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''Email
makeLenses ''Email

newtype SubscriberId = SubscriberId { _unSubscriberId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''SubscriberId
makeLenses ''SubscriberId

incSubscriberId :: SubscriberId -> SubscriberId
incSubscriberId (SubscriberId i) = SubscriberId (i + 1)

data SubscriptionStatus
  = Subscribed
  | AwaitingConfirmation UUID
  | Bouncing
  | Unsubscribed
    deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''SubscriptionStatus

data Subscriber = Subscriber
   { _subId     :: SubscriberId
   , _subEmail  :: Email
   , _subStatus :: [(UTCTime, SubscriptionStatus)] -- sort newest to oldest -- current status is the first element of the list
   }
   deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''Subscriber
makeLenses ''Subscriber

instance Indexable Subscriber where
    empty = ixSet [ ixFun $ (:[]) . _subId
                  , ixFun $ (:[]) . _subEmail
                  , ixFun $ map snd . take 1 . _subStatus -- current subscription status
                  ]

-- not to be confused with message-id header
newtype MessageId = MessageId { _unMessageId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo, Enum)
deriveSafeCopy 0 'base ''MessageId
makeLenses ''MessageId

data MessageStatus
    = Draft
    | Sent UTCTime
    | Scheduled UTCTime
    deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''MessageStatus

data Message = Message
    { _msgId      :: MessageId
    , _msgFrom    :: Email
    , _msgSubject :: Text
    , _msgBody    :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''Message
makeLenses ''Message

instance Indexable Message where
  empty = ixSet $ [ ixFun $ (:[]) . _msgId
                  ]

emailToAddress :: Email -> Address
emailToAddress (Email eml) = Address Nothing eml

sendStringTemplateEmail :: [(String, Text)] -> Message -> Email -> Mail
sendStringTemplateEmail attrs message toEmail =
  simpleMail' (toEmail ^. to emailToAddress ) (message ^. msgFrom ^. to emailToAddress) (message ^. msgSubject) (render $ setManyAttrib attrs $ newSTMP $ message ^. msgBody ^. to unpack)
