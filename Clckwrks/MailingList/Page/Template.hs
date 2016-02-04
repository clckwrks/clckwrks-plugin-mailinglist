{-# LANGUAGE FlexibleContexts, QuasiQuotes, OverloadedStrings #-}
module Clckwrks.MailingList.Page.Template where

import Clckwrks
import Clckwrks.MailingList.Monad
import Clckwrks.MailingList.URL
import Clckwrks.Plugin
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text)
import Language.Haskell.HSX.QQ (hsx)
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
       hdrXml <- fmap (map unClckChild) $ unXMLGenT $ asChild [hsx| <%> <link rel="stylesheet" type="text/css" href=(MailingListData "style.css") /> <% hdrs %></%> |]
       bdyXml <- fmap (map unClckChild) $ unXMLGenT $ asChild bdy
       clckT2MailingListT $ themeTemplate p (ThemeStyleId 0) ttl hdrXml bdyXml
