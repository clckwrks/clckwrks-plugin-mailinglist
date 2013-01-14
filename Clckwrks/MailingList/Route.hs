module Clckwrks.MailingList.Route where

import Control.Applicative          ((<$>))
import Control.Monad.Reader         (ask)
import Control.Monad.Trans          (liftIO)
import Clckwrks                     (Role(..), requiresRole_)
import Clckwrks.MailingList.Monad          (MailingListM, MailingListConfig(..))
import Clckwrks.MailingList.URL            (MailingListURL(..), MailingListAdminURL(..))
import Clckwrks.MailingList.Page.Subscribe   (subscribePage)
import qualified Data.Set           as Set
import Happstack.Server             (Response, notFound, toResponse, serveFile, guessContentTypeM, mimeTypes)
import Happstack.Server.FileServe.BuildingBlocks (isSafePath)
import Network.URI                  (unEscapeString)
import System.FilePath              ((</>), makeRelative, splitDirectories)
import Paths_clckwrks_plugin_bugs   (getDataDir)

checkAuth :: MailingListURL
          -> MailingListM MailingListURL
checkAuth url =
    do showFn <- mailingListClckURL <$> ask
       let requiresRole = requiresRole_ showFn
       case url of
         MailingListAdmin  {} -> requiresRole (Set.singleton Administrator) url
         Subscribe         {} -> return url

routeMailingList :: MailingListURL
                 -> MailingListM Response
routeMailingList unsecureURL =
    do url <- checkAuth unsecureURL
       case url of
         Subscribe     -> subscribePage url
