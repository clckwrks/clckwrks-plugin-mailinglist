{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.MailingList.Admin.ViewSubscribers where

import Clckwrks                    (query, update)
import Clckwrks.Admin.Template     (template)
import Clckwrks.MailingList.Acid   (AskSubscribers(..))
import Clckwrks.MailingList.Monad  (MailingListConfig(mailingListClckURL), MailingListM)
import Clckwrks.MailingList.Types  (Subscriber, SubscriberId, subId, unSubscriberId, subEmail, unEmail, subStatus)
import Clckwrks.MailingList.URL
import Control.Monad.Trans         (liftIO)
import Control.Lens                ((^.))
import Data.Maybe                  (fromMaybe, maybe)
import Data.Monoid                 (mempty)
import qualified Data.IxSet        as IxSet
import Data.Text                   (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as L
import Language.Haskell.HSX.QQ     (hsx)
import Happstack.Server            (Response, seeOther, toResponse)
import HSP
import System.Directory            (doesFileExist)
import Text.Html.Email.Validate (isValidEmail)
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transformEitherM, transform, mapView)
import qualified Text.Reform.Generalized  as G
import Text.Reform.Happstack (reform)
import Text.Reform.HSP.Text (form, inputEmail, inputText, setAttrs, label, labelText, inputSubmit, errorList, textarea, fieldset)
import Web.Routes (showURL)

viewSubscribers :: MailingListURL -> MailingListM Response
viewSubscribers here =
  do subs <- query AskSubscribers
     template "mailing list subscribers" () $
       [hsx| <%>
         <h1>Subscribers</h1>
         <table class="table">
          <thead>
           <tr><th>ID</th><th>Email</th><th>Status</th></tr>
          </thead>
          <tbody>
           <% mapM showSubscriber (IxSet.toAscList (IxSet.Proxy :: IxSet.Proxy SubscriberId) subs) %>
          </tbody>
          </table>
       </%> |]
       where
         showSubscriber :: Subscriber -> MailingListM XML
         showSubscriber sub=
           unXMLGenT $ [hsx|
               <tr>
                <td><% show $ sub ^. subId ^. unSubscriberId %></td>
                <td><% sub ^. subEmail ^. unEmail %></td>
                <td><% show $ head $  sub ^. subStatus %></td>
               </tr>
               |]
