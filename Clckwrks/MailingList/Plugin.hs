{-# LANGUAGE RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Clckwrks.MailingList.Plugin where

import Clckwrks
import Clckwrks.Plugin
import Clckwrks.MailingList.URL
import Clckwrks.MailingList.Acid
import Clckwrks.MailingList.PreProcess    (mailingListCmd)
import Clckwrks.MailingList.Monad
import Clckwrks.MailingList.Route
import Control.Monad.State         (get)
import Data.Acid.Local
import Data.Text                   (Text)
import qualified Data.Text.Lazy    as TL
import Data.Maybe                  (fromMaybe)
import System.FilePath             ((</>))
import Web.Plugins.Core            (Plugin(..), When(Always), addCleanup, addHandler, getConfig, getPluginRouteFn, getPluginState, initPlugin)

mailingListHandler :: (MailingListURL -> [(Text, Maybe Text)] -> Text)
            -> MailingListConfig
            -> ClckPlugins
            -> [Text]
            -> ClckT ClckURL (ServerPartT IO) Response
mailingListHandler showMailingListURL mailingListConfig plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) ->
          ClckT $ withRouteT flattenURL $ unClckT $ runMailingListT mailingListConfig $ routeMailingList u
    where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (MailingListURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showMailingListURL u p

mailingListInit :: ClckPlugins
         -> IO (Maybe Text)
mailingListInit plugins =
    do (Just mailingListShowFn) <- getPluginRouteFn plugins (pluginName mailingListPlugin)
       (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       mTopDir <- clckTopDir <$> getConfig plugins
       let basePath = maybe "_state" (\td -> td </> "_state") mTopDir -- FIXME
       acid <- openLocalStateFrom (basePath </> "mailingList") initialMailingListState
       addCleanup plugins Always (createCheckpointAndClose acid)
       let mailingListConfig = MailingListConfig
               { mailingListState     = acid
               , mailingListClckURL   = clckShowFn
               }
       addPreProc plugins (\txt-> runMailingListT'' mailingListShowFn mailingListConfig $ mailingListCmd mailingListShowFn txt)
       addHandler plugins (pluginName mailingListPlugin) (mailingListHandler mailingListShowFn mailingListConfig)
       return Nothing
{-
addMailingListAdminMenu :: ClckT url IO ()
addMailingListAdminMenu =
    do p <- plugins <$> get
       (Just showMailingListURL) <- getPluginRouteFn p (pluginName mailingListPlugin)
       let editMilestonesURL = showMailingListURL (MailingListAdmin EditMilestones) []
       addAdminMenu ("MailingList", [("Edit Milestones", editMilestonesURL)])
-}
runMailingListT' :: MonadIO m =>
                    ClckPlugins
                 -> MailingListT m a
                 -> ClckT url m a
runMailingListT' plugins m =
    do mMailingListConfig <- liftIO $ getPluginState plugins (pluginName mailingListPlugin)
       mRouteFn      <- liftIO $ getPluginRouteFn plugins (pluginName mailingListPlugin)
       case (mMailingListConfig, mRouteFn) of
         (Just mailingListConfig, Just routeFn) ->
             runMailingListT'' routeFn mailingListConfig m



mailingListPlugin :: Plugin MailingListURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig [TL.Text -> ClckT ClckURL IO TL.Text]
mailingListPlugin = Plugin
    { pluginName       = "mailingList"
    , pluginInit       = mailingListInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = return () -- addMailingListAdminMenu
    }

plugin :: ClckPlugins -- ^ plugins
       -> Text        -- ^ baseURI
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI mailingListPlugin