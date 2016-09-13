{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.MailingList.Admin.ViewMessages where

import Clckwrks                    (query, update)
import Clckwrks.Admin.Template     (template)
import Clckwrks.MailingList.Acid   (AskMessageSubjects(..))
import Clckwrks.MailingList.Monad  (MailingListConfig(mailingListClckURL), MailingListM)
import Clckwrks.MailingList.Types  (MessageId, Subscriber, SubscriberId, subId, unSubscriberId, subEmail, unEmail, subStatus, unMessageId)
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

viewMessages :: MailingListURL -> MailingListM Response
viewMessages here =
  do msgs <- query AskMessageSubjects
     template "mailing list messages" () $
       [hsx| <%>
         <h1>Messages</h1>
         <table class="table">
          <thead>
           <tr><th>ID</th><th>Subject</th><th>Edit</th><th>Send</th></tr>
          </thead>
          <tbody>
           <% mapM showMessage msgs %>
          </tbody>
          </table>
       </%> |]
       where
         showMessage :: (MessageId, Text) -> MailingListM XML
         showMessage (msgid, subj) = do
           editURL <- showURL (MailingListAdmin $ EditMessage msgid)
           sendURL <- showURL (MailingListAdmin $ SendMessage msgid)
           unXMLGenT $ [hsx|
               <tr>
                <td><% show $ msgid ^. unMessageId %></td>
                <td><% subj %></td>
                <td><a href=editURL>edit</a></td>
                <td><a href=sendURL>send</a></td>
               </tr>
               |]
