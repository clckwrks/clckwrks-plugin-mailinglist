{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.MailingList.Page.Template where

import Clckwrks
import Clckwrks.MailingList.Monad
import Clckwrks.MailingList.URL
import Clckwrks.Plugin
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text)
import HSP hiding (escape)
import Happstack.Server.HSP.HTML ()
import Web.Plugins.Core (Plugin(..), getPluginRouteFn, getTheme)

template :: ( EmbedAsChild MailingListM headers
            , EmbedAsChild MailingListM body
            ) =>
            Text
         -> headers
         -> body
         -> MailingListM Response
template ttl hdrs bdy =
    do p <- plugins <$> get
       mTheme <- getTheme p
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       case mTheme of
         Nothing      -> escape $ internalServerError $ toResponse $ ("No theme package is loaded." :: Text)
         (Just theme) ->
             do hdrXml <- fmap (map unClckChild) $ unXMLGenT $ asChild <%> <link rel="stylesheet" type="text/css" href=(MailingListData "style.css") /> <% hdrs %></%>
                bdyXml <- fmap (map unClckChild) $ unXMLGenT $ asChild bdy
                fmap toResponse $ mapClckT f $ ClckT $ withRouteT (\f -> clckShowFn) $ unClckT $ unXMLGenT $ (_themeTemplate theme ttl hdrXml bdyXml)
    where
      f :: ServerPartT IO (a, ClckState) -> ReaderT MailingListConfig (ServerPartT IO) (a, ClckState)
      f m = ReaderT $ \_ -> m
