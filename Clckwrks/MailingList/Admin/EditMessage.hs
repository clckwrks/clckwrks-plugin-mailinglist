{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.MailingList.Admin.EditMessage where

import Clckwrks                    (query, update, notFound, seeOtherURL)
import Clckwrks.Admin.Template     (template)
import Clckwrks.MailingList.Acid   (MessageById(..), UpdateMessage(..))
import Clckwrks.MailingList.Monad  (MailingListConfig(mailingListClckURL), MailingListM, MailingListForm, MailingListFormError(InvalidEmail, MissingSubject, MissingLink, SendmailNotFound))
import Clckwrks.MailingList.Types  (Email(..), Message(..), MessageId, msgId, msgFrom, msgSubject, msgBody, unEmail, unMessageId)
import Clckwrks.MailingList.URL
import Control.Monad.Trans         (MonadIO, liftIO)
import Control.Lens                ((^.), (.~), (&))
import Data.Maybe                  (fromMaybe, maybe)
import Data.Monoid                 (mempty)
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

editMessage :: MailingListURL -> MessageId -> MailingListM Response
editMessage here msgid =
  do mMessage <- query (MessageById msgid)
     case mMessage of
       Nothing -> notFound $ toResponse $ "Message not found: "  ++ show (msgid ^. unMessageId)
       (Just message) ->
         do action <- showURL here
            template "edit message" () $ [hsx|
             <%>
              <% reform (form action) "em" updateMsg Nothing (messageForm message) %>
             </%> |]
  where
    updateMsg :: Message -> MailingListM Response
    updateMsg msg =
      do update (UpdateMessage msg)
         seeOtherURL here

messageForm :: Message -> MailingListForm Message
messageForm msg =
      divHorizontal $
      (fieldset $
        (,,)
          <$> (divControlGroup (label' "From"           ++> (divControls $ inputText (msg ^. msgFrom ^. unEmail) `setAttrs` [("size" := "80"), ("class" := "input-xxlarge") :: Attr Text Text])))
          <*> (divControlGroup (label' "Subject"           ++> (divControls $ inputText (msg ^. msgSubject) `setAttrs` [("size" := "80"), ("class" := "input-xxlarge") :: Attr Text Text])))
          <*> (divControlGroup (label' "Body"            ++> (divControls $ textarea 80 25 (msg ^. msgBody) `setAttrs` [("class" := "input-xxlarge")  :: Attr Text Text])))
          <* (divFormActions (inputSubmit' (T.pack "Save")))
      ) `transformEitherM` toMessage
  where
    inputSubmit' :: Text -> MailingListForm (Maybe Text)
    inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn") :: Attr Text Text]
--     inputCheckboxLabel :: Text -> Bool -> MailingListForm Bool
--     inputCheckboxLabel lbl b = mapView (\xml -> [[hsx| <label class="checkbox"><% xml %><% lbl %></label>|]]) (inputCheckbox b)

    label' :: L.Text -> MailingListForm ()
    label' str       = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])

    labelCB :: L.Text -> MailingListForm ()
    labelCB str      = labelText str `setAttrs` [("class":="checkbox") :: Attr Text Text]
--      divInline        = mapView (\xml -> [<div class="checkbox inline"><% xml %></div>])
    divFormActions   = mapView (\xml -> [[hsx|<div class="form-actions"><% xml %></div>|]])
    divHorizontal    = mapView (\xml -> [[hsx|<div class="form-horizontal"><% xml %></div>|]])
    divControlGroup  = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
    divControls      = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])
    toMessage :: (MonadIO m) => (Text, Text, Text) -> m (Either MailingListFormError Message)
    toMessage (from, subj, bdy) =
        pure $ Right $ msg & msgFrom .~ (Email from)
                           & msgSubject .~ subj
                           & msgBody    .~ bdy
