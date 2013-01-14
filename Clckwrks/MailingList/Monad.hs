{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeSynonymInstances, UndecidableInstances, OverloadedStrings #-}
module Clckwrks.MailingList.Monad where

import Clckwrks                 (Clck, ClckT(..), ClckFormT, ClckState(..), ClckURL(..), mapClckT, withRouteT)
import Clckwrks.Acid
import Clckwrks.IOThread        (IOThread(..), startIOThread, killIOThread)
import Clckwrks.MailingList.Acid
import Clckwrks.MailingList.Types
import Clckwrks.MailingList.URL
import Control.Applicative ((<$>))
import Control.Exception   (bracket)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Data.Acid           (AcidState)
import Data.Acid.Local     (createCheckpointAndClose, openLocalStateFrom)
import Data.Typeable       (Typeable)
import qualified Data.Map  as Map
import Data.Maybe          (fromMaybe)
import qualified Data.Text as T
import Happstack.Server
import Happstack.Server.Internal.Monads (FilterFun)
import HSP                  (Attr((:=)), Attribute(MkAttr), EmbedAsAttr(..), EmbedAsChild(..), IsName(toName), XMLGenT, XML, pAttrVal)
import System.Directory     (createDirectoryIfMissing)
import System.FilePath      ((</>))
import Text.Reform          (CommonFormError, FormError(..))
import Web.Routes           (URL, MonadRoute, showURL)

data MailingListConfig = MailingListConfig
    { mailingListState        :: AcidState MailingListState
    , mailingListClckURL      :: ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text
    }
    deriving Typeable

type MailingListT m = ClckT MailingListURL (ReaderT MailingListConfig m)
type MailingListM   = ClckT MailingListURL (ReaderT MailingListConfig (ServerPartT IO))

data MailingListFormError
    = MailingListCFE (CommonFormError [Input])
    | InvalidEmail
      deriving Show

instance FormError MailingListFormError where
    type ErrorInputType MailingListFormError = [Input]
    commonFormError = MailingListCFE

instance (Functor m, Monad m) => EmbedAsChild (MailingListT m) MailingListFormError where
    asChild InvalidEmail = asChild "This is definitely not a valid email address."
    asChild e            = asChild (show e)

type MailingListForm = ClckFormT MailingListFormError MailingListM

instance (IsName n) => EmbedAsAttr MailingListM (Attr n MailingListURL) where
        asAttr (n := u) =
            do url <- showURL u
               asAttr $ MkAttr (toName n, pAttrVal (T.unpack url))

instance (IsName n) => EmbedAsAttr MailingListM (Attr n ClckURL) where
        asAttr (n := url) =
            do showFn <- mailingListClckURL <$> ask
               asAttr $ MkAttr (toName n, pAttrVal (T.unpack $ showFn url []))

runMailingListT :: MailingListConfig -> MailingListT m a -> ClckT MailingListURL m a
runMailingListT mc m = mapClckT f m
    where
      f r = runReaderT r mc

runMailingListT'' :: Monad m =>
                     (MailingListURL -> [(T.Text, Maybe T.Text)] -> T.Text)
                  -> MailingListConfig
                  -> MailingListT m a
                  -> ClckT url m a
runMailingListT'' showMailingListURL stripeConfig m = ClckT $ withRouteT flattenURL $ unClckT $ runMailingListT stripeConfig $ m
    where
      flattenURL ::   ((url' -> [(T.Text, Maybe T.Text)] -> T.Text) -> (MailingListURL -> [(T.Text, Maybe T.Text)] -> T.Text))
      flattenURL _ u p = showMailingListURL u p


instance (Monad m) => MonadReader MailingListConfig (MailingListT m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (MailingListT m) MailingListState where
    getAcidState =
        mailingListState <$> ask
