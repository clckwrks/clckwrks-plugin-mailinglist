{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Clckwrks.MailingList.Route where

import Control.Applicative          ((<$>))
import Control.Monad.Reader         (ask)
import Control.Monad.Trans          (liftIO)
import Clckwrks                     (Role(..), requiresRole_)
import Clckwrks.MailingList.Admin.EditMessage (editMessage)
import Clckwrks.MailingList.Admin.MailingListSettings (mailingListSettings)
import Clckwrks.MailingList.Admin.SendMessage(sendMessage)
import Clckwrks.MailingList.Admin.ViewMessages (viewMessages)
import Clckwrks.MailingList.Admin.ViewSubscribers (viewSubscribers)
import Clckwrks.MailingList.Monad          (MailingListM, MailingListConfig(..))
import Clckwrks.MailingList.URL            (MailingListURL(..), MailingListAdminURL(..))
import Clckwrks.MailingList.Page.Subscribe   (subscribePage)
import Clckwrks.MailingList.Page.ConfirmOptIn   (confirmOptInPage)
import qualified Data.Set           as Set
import Happstack.Server             (Response, notFound, toResponse, serveFile, guessContentTypeM, mimeTypes)
import Happstack.Server.FileServe.BuildingBlocks (isSafePath)
import Network.URI                  (unEscapeString)
import System.FilePath              ((</>), makeRelative, splitDirectories)

checkAuth :: MailingListURL
          -> MailingListM MailingListURL
checkAuth url =
    do showFn <- mailingListClckURL <$> ask
       let requiresRole = requiresRole_ showFn
       case url of
         MailingListAdmin  {} -> requiresRole (Set.singleton Administrator) url
         Subscribe         {} -> return url
         ConfirmOptIn      {} -> return url

-- | routes for 'AdminURL'
routeMailingListAdmin :: MailingListAdminURL -> MailingListM Response
routeMailingListAdmin url =
    case url of
      EditMLSettings      -> mailingListSettings (MailingListAdmin url)
      ViewSubscribers     -> viewSubscribers     (MailingListAdmin url)
      (SendMessage msgid) -> sendMessage (MailingListAdmin url) msgid
      (EditMessage msgid) -> editMessage (MailingListAdmin url) msgid
      ViewMessages        -> viewMessages (MailingListAdmin url)

routeMailingList :: MailingListURL
                 -> MailingListM Response
routeMailingList unsecureURL =
    do url <- checkAuth unsecureURL
       case url of
         Subscribe                   -> subscribePage url
         ConfirmOptIn                -> confirmOptInPage
         (MailingListAdmin adminURL) -> routeMailingListAdmin adminURL

