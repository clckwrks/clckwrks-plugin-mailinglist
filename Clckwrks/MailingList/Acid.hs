{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Clckwrks.MailingList.Acid where

import Control.Applicative    ((<$>))
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, modify, put)
import Data.Acid              (Query, Update, makeAcidic)
import Data.Data              (Data, Typeable)
import Data.IxSet             (IxSet, Proxy(..), (@=), (@+), getOne, empty, toAscList, toList, fromList, updateIx)
import qualified Data.IxSet   as IxSet
import Data.Map               (Map)
import qualified Data.Map     as Map
import Data.Ratio             ((%))
import Data.SafeCopy          (base, deriveSafeCopy, extension, Migrate(..))
import Data.Set               as Set
import           Data.Text    (Text)
import qualified Data.Text    as Text
import Data.Time              (UTCTime(..))
import Clckwrks.MailingList.Types (Email(..), Subscriber(..), SubscriberId(..), incSubscriberId)


data MailingListState = MailingListState
    { subscribers      :: IxSet Subscriber
    , nextSubscriberId :: SubscriberId
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''MailingListState)

initialMailingListState :: MailingListState
initialMailingListState = MailingListState
    { subscribers      = IxSet.empty
    , nextSubscriberId = SubscriberId 1
    }

addSubscriber :: Email -> UTCTime -> Update MailingListState Subscriber
addSubscriber email now =
    do ms@MailingListState{..} <- get
       let subscriber = Subscriber { subId    = nextSubscriberId
                                   , subEmail = email
                                   , subDate  = now
                                   }
       put $ ms { subscribers      = IxSet.insert subscriber subscribers
                , nextSubscriberId = incSubscriberId nextSubscriberId
                }
       return subscriber

$(makeAcidic ''MailingListState
  [ 'addSubscriber
  ])
