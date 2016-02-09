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
import Control.Monad.State (get)
import Control.Monad.Trans (MonadIO, lift)
import Clckwrks.Plugin               (clckPlugin)
import Data.Acid           (AcidState)
import Data.Acid.Local     (createCheckpointAndClose, openLocalStateFrom)
import Data.Typeable       (Typeable)
import qualified Data.Map  as Map
import Data.Maybe          (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Happstack.Server
import Happstack.Server.Internal.Monads (FilterFun)
import HSP                  (Attr((:=)), Attribute(MkAttr), EmbedAsAttr(..), EmbedAsChild(..), IsName(toName), XMLGenT, XML, pAttrVal)
import System.Directory     (createDirectoryIfMissing)
import System.FilePath      ((</>))
import Text.Reform          (CommonFormError, FormError(..))
import Web.Plugins.Core (Plugin(..), getConfig, getPluginsSt, getPluginRouteFn)
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
    | MissingLink
    | MissingSubject
    | SendmailNotFound
      deriving Show

instance FormError MailingListFormError where
    type ErrorInputType MailingListFormError = [Input]
    commonFormError = MailingListCFE

instance (Functor m, Monad m) => EmbedAsChild (MailingListT m) MailingListFormError where
    asChild InvalidEmail = asChild ("This is definitely not a valid email address." :: T.Text)
    asChild e            = asChild (show e)

type MailingListForm = ClckFormT MailingListFormError MailingListM

instance (IsName n TL.Text) => EmbedAsAttr MailingListM (Attr n MailingListURL) where
        asAttr (n := u) =
            do url <- showURL u
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict url))

instance (IsName n TL.Text) => EmbedAsAttr MailingListM (Attr n ClckURL) where
        asAttr (n := url) =
            do showFn <- mailingListClckURL <$> ask
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict $ showFn url []))

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

flattenURLClckT :: (url1 -> [(T.Text, Maybe T.Text)] -> T.Text)
                -> ClckT url1 m a
                -> ClckT url2 m a
flattenURLClckT showClckURL m = ClckT $ withRouteT flattenURL $ unClckT m
    where
      flattenURL _ = \u p -> showClckURL u p

clckT2MailingListT :: (Functor m, MonadIO m, Typeable url1) =>
             ClckT url1 m a
          -> MailingListT m a
clckT2MailingListT m =
    do p <- plugins <$> get
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       flattenURLClckT clckShowFn $ mapClckT addReaderT m
    where
      addReaderT :: (Monad m) => m (a, ClckState) -> ReaderT MailingListConfig m (a, ClckState)
      addReaderT m =
          do (a, cs) <- lift m
             return (a, cs)
