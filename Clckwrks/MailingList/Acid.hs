{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.MailingList.Acid where

import Control.Lens  ((^.), (.=), (?=), (^?), view, set, over)
import Control.Lens.At (at)
import Control.Applicative    ((<$>))
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
import Data.Set               as Set
import           Data.Text    (Text)
import qualified Data.Text    as Text
import Data.Time              (UTCTime(..))
import Data.UUID              (UUID)
import Clckwrks.MailingList.Types (Email(..), Message(..), msgId, MessageId(..), Subscriber(..), subId, subStatus, SubscriptionStatus(..), SubscriberId(..), incSubscriberId)

data MailingListState = MailingListState
    { _subscribers       :: IxSet Subscriber
    , _nextSubscriberId  :: SubscriberId
    , _messages          :: Map MessageId Message
    , _nextMessageId     :: MessageId
    , _contactAddr       :: Maybe Email
    , _optInConfirm      :: Maybe MessageId
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''MailingListState
makeLenses ''MailingListState

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
    , _nextMessageId    = MessageId 1
    , _contactAddr      = Nothing
    , _optInConfirm     = Nothing
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

getMailingListSettings :: Query MailingListState (Maybe Email, Maybe Message)
getMailingListSettings =
  do mEmail <- view contactAddr
     mId <- view optInConfirm
     case mId of
      Nothing -> pure (mEmail, Nothing)
      (Just mid) ->
        do mMessage <- view (messages . at mid)
           pure (mEmail, mMessage)

setMailingListSettings :: Maybe Email
                       -> Maybe Message
                       -> Update MailingListState ()
setMailingListSettings mEmail mMessage =
  do contactAddr .= mEmail
     case mMessage of
       Nothing -> do
         optInConfirm .= Nothing
       (Just message) -> do
         optInConfirm .= over _Just (^. msgId) mMessage
         messages . at (message ^. msgId) ?= message

$(makeAcidic ''MailingListState
  [ 'addSubscriber
  , 'changeSubscriptionStatus
  , 'getOptInConfirmMessage
  , 'signupSubscriber
  , 'verifyOptIn
  , 'getMailingListSettings
  , 'setMailingListSettings
  ])
