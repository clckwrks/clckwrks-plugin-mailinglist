{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.MailingList.Acid where

import Control.Lens  ((^.), (.=), (?=), (^?), (&), (.~), view, set, over)
import Control.Lens.At (at)
import Control.Applicative    ((<$>))
import Control.Lens.At        (at)
import Control.Lens.TH        (makeLenses)
import Control.Lens.Prism     (_Just)
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, modify, put)
import Data.Acid              (Query, Update, makeAcidic)
import Data.Data              (Data, Typeable)
import Data.IxSet             (IxSet, Proxy(..), (@=), (@+), getOne, empty, toAscList, toList, fromList, updateIx)
import qualified Data.IxSet   as IxSet
import Data.Map               (Map)
import qualified Data.Map     as Map
import Data.Monoid            ((<>))
import Data.Ratio             ((%))
import Data.SafeCopy          (base, deriveSafeCopy, extension, Migrate(..))
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Text    (Text)
import qualified Data.Text    as Text
import Data.Time              (UTCTime(..))
import Data.UUID              (UUID)
import Clckwrks.MailingList.Types (Email(..), Message(..), msgId, MessageId(..), Subscriber(..), subId, subStatus, SubscriptionStatus(..), SubscriberId(..), incSubscriberId, msgSubject, msgId)
import System.FilePath        (FilePath)

data MailingListState_0 = MailingListState_0
    { _subscribers_0       :: IxSet Subscriber
    , _nextSubscriberId_0  :: SubscriberId
    , _messages_0          :: Map MessageId Message
    , _nextMessageId_0     :: MessageId
    , _contactAddr_0       :: Maybe Email
    , _optInConfirm_0      :: Maybe MessageId
    , _sendmailPath_0      :: Maybe FilePath
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''MailingListState_0

data MailingListState = MailingListState
    { _subscribers       :: IxSet Subscriber
    , _nextSubscriberId  :: SubscriberId
    , _messages          :: Map MessageId Message
    , _mailLog           :: Set (MessageId, SubscriberId, UTCTime)
    , _nextMessageId     :: MessageId
    , _contactAddr       :: Maybe Email
    , _optInConfirm      :: Maybe MessageId
    , _sendmailPath      :: Maybe FilePath
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 1 'extension ''MailingListState
makeLenses ''MailingListState

instance Migrate MailingListState where
  type MigrateFrom MailingListState = MailingListState_0
  migrate (MailingListState_0 s ns m nm ca oic sp) = MailingListState s ns m Set.empty nm ca oic sp

optInConfirmMsgId :: MessageId
optInConfirmMsgId = MessageId 1

optInConfirmMsg :: Email -> Message
optInConfirmMsg from =
  Message { _msgId = MessageId 1
          , _msgSubject = "Please Confirm Your Mailing List Subscription"
          , _msgFrom = from
          , _msgBody = "Hello,\n\n\
                        \\
                        \To complete your subscription to our mailing list you must click on the following link:\n\n" <> "$link$" <>
                       "\n\nIf you did not issue the request to join this list then you can just ignore this message and you will remain unsubscribed."
          }

initialMailingListState :: MailingListState
initialMailingListState = MailingListState
    { _subscribers      = IxSet.empty
    , _nextSubscriberId = SubscriberId 1
    , _messages         = Map.empty
    , _mailLog          = Set.empty
    , _nextMessageId    = MessageId 2
    , _contactAddr      = Nothing
    , _optInConfirm     = Nothing
    , _sendmailPath     = Nothing
--    , _sendQueue        = [(SubscriberId, MessageId)]
--    , _mailLog          = Set (SubscriberId, MessageId, UTCTime)
    }

-- | FIXME: behavior if email already exists
addSubscriber :: Email -> SubscriptionStatus -> UTCTime -> Update MailingListState Subscriber
addSubscriber email subscriptionStatus now =
    do ms@MailingListState{..} <- get
       case getOne $ (ms ^. subscribers) @= email of
        Nothing -> do
          let subscriber = Subscriber { _subId     = _nextSubscriberId
                                      , _subEmail  = email
                                      , _subStatus = [(now, subscriptionStatus)]
                                      }
          put $ ms { _subscribers      = IxSet.insert subscriber _subscribers
                   , _nextSubscriberId = incSubscriberId _nextSubscriberId
                   }
          pure subscriber
        (Just subscriber) ->
          pure subscriber

askSubscribers :: Query MailingListState (IxSet Subscriber)
askSubscribers =
  do ms <- ask
     pure $ ms ^. subscribers
{-
askActiveSubscribers :: Query MailingListState (IxSet Subscriber)
askActiveSubscribers =
  do ms <- ask
     case (ms ^. subscribers) @= Subscribed of
       subs -> pure $ filter (\sub -> head (sub ^. subStatus) == Subscribed) IxSet.toList
-}
signupSubscriber :: Email -> SubscriptionStatus -> UTCTime -> Update MailingListState Subscriber
signupSubscriber email subscriptionStatus now =
    do ms@MailingListState{..} <- get
       case getOne $ (ms ^. subscribers) @= email of
        -- email not already in the system
        Nothing -> do
          let subscriber = Subscriber { _subId     = _nextSubscriberId
                                      , _subEmail  = email
                                      , _subStatus = [(now, subscriptionStatus)]
                                      }
          put $ ms { _subscribers      = IxSet.insert subscriber _subscribers
                   , _nextSubscriberId = incSubscriberId _nextSubscriberId
                   }
          pure subscriber
        (Just subscriber) ->
          case subscriber ^. subStatus of
           -- not sure how this would happen
           [] -> do
             let subscriber' = set subStatus [(now, subscriptionStatus)] subscriber
             put $ ms { _subscribers = IxSet.updateIx (subscriber' ^. subId) subscriber' _subscribers }
             pure subscriber'
           statuses@(currentStatus:previousStatuses) ->
             case currentStatus of
               (_, Subscribed) -> pure subscriber
               (_, AwaitingConfirmation{}) -> pure subscriber
               _ -> do
                 let subscriber' = set subStatus ((now, subscriptionStatus):statuses) subscriber
                 put $ ms { _subscribers = IxSet.updateIx (subscriber' ^. subId) subscriber' _subscribers }
                 pure subscriber'

data VerificationResult
  = SubscriptionConfirmed
  | AlreadySubscribed
  | InvalidConfirmation
deriveSafeCopy 0 'base ''VerificationResult

verifyOptIn :: UTCTime -> SubscriberId -> UUID -> Update MailingListState VerificationResult
verifyOptIn now sid uuid =
  do ms@MailingListState{..} <- get
     case getOne $ (ms ^. subscribers) @= sid of
      Nothing -> pure InvalidConfirmation
      (Just subscriber) ->
        case subscriber ^. subStatus of
         ((_,AwaitingConfirmation uuid'):_)
           | uuid == uuid' ->
               do let subscriber' = subscriber { _subStatus = (now, Subscribed) : (_subStatus subscriber) }
                  put $ ms { _subscribers = IxSet.updateIx sid subscriber' _subscribers }
                  pure SubscriptionConfirmed
           | otherwise -> pure InvalidConfirmation
         ((_,Subscribed):_) -> pure AlreadySubscribed
         _ -> pure InvalidConfirmation

changeSubscriptionStatus :: SubscriberId -> SubscriptionStatus -> UTCTime -> Update MailingListState (Maybe Subscriber)
changeSubscriptionStatus subscriberId newStatus now =
  do ms@MailingListState{..} <- get
     case getOne $ _subscribers @= subscriberId of
      Nothing -> pure Nothing
      (Just subscriber) ->
        do let subscriber' = subscriber { _subStatus = (now, newStatus) : (_subStatus subscriber) }
           put $ ms { _subscribers = IxSet.updateIx subscriberId subscriber' _subscribers }
           pure $ Just subscriber'

getOptInConfirmMessage :: Query MailingListState (Maybe Message)
getOptInConfirmMessage =
  do mmessageId <- view optInConfirm
     case mmessageId of
      Nothing -> pure Nothing
      (Just messageId) ->
        view (messages . at messageId)

getMailingListSettings :: Query MailingListState (Maybe FilePath, Maybe Email, Maybe Message)
getMailingListSettings =
  do mSendmail <- view sendmailPath
     mEmail <- view contactAddr
     mId <- view optInConfirm
     case mId of
      Nothing -> pure (mSendmail, mEmail, Nothing)
      (Just mid) ->
        do mMessage <- view (messages . at mid)
           pure (mSendmail, mEmail, mMessage)

getSendmailPath :: Query MailingListState (Maybe FilePath)
getSendmailPath = view sendmailPath

setMailingListSettings :: Maybe FilePath
                       -> Maybe Email
                       -> Maybe Message
                       -> Update MailingListState ()
setMailingListSettings mSendmail mEmail mMessage =
  do sendmailPath .= mSendmail
     contactAddr .= mEmail
     case mMessage of
       Nothing -> do
         optInConfirm .= Nothing
       (Just message) -> do
         optInConfirm .= over _Just (^. msgId) mMessage
         messages . at (message ^. msgId) ?= message

addMailLogEntry :: (MessageId, SubscriberId, UTCTime) -> Update MailingListState ()
addMailLogEntry entry =
  do mls@MailingListState {..} <- get
     put $ mls { _mailLog = Set.insert entry _mailLog }

askMessageSubjects :: Query MailingListState [(MessageId, Text)]
askMessageSubjects =
  do mls <- ask
     pure $ map (\(i, m) -> (i, m ^. msgSubject)) (Map.toList $ mls ^. messages)

messageById :: MessageId -> Query MailingListState (Maybe Message)
messageById msgid =
  view (messages . at msgid)

updateMessage :: Message -> Update MailingListState ()
updateMessage msg =
  messages . at (msg ^. msgId) .= Just msg

createMessage :: Update MailingListState MessageId
createMessage =
  do mls <- get
     let msgid = mls ^. nextMessageId
         msg = Message msgid (Email mempty) mempty mempty
     put $ mls & (messages . at msgid) .~ Just msg
               & nextMessageId .~ succ msgid
     pure msgid

$(makeAcidic ''MailingListState
  [ 'addSubscriber
  , 'addMailLogEntry
  , 'askSubscribers
--  , 'askActiveSubscriber
  , 'changeSubscriptionStatus
  , 'getOptInConfirmMessage
  , 'signupSubscriber
  , 'verifyOptIn
  , 'getMailingListSettings
  , 'setMailingListSettings
  , 'getSendmailPath
  , 'askMessageSubjects
  , 'messageById
  , 'updateMessage
  , 'createMessage
  ])
