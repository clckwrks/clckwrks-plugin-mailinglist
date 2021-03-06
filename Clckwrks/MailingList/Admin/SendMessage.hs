{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.MailingList.Admin.SendMessage where

import Clckwrks                    (query, update)
import Clckwrks.Admin.Template     (template)
import Clckwrks.MailingList.Acid   (MessageById(..))
import Clckwrks.MailingList.Monad  (MailingListConfig(mailingListClckURL), MailingListM, MailingListForm, MailingListFormError(InvalidEmail, MissingSubject, MissingLink, SendmailNotFound))
import Clckwrks.MailingList.Types  (Email(..), Message(..), MessageId, msgId, msgFrom, msgSubject, msgBody, unEmail, unMessageId)
import Clckwrks.MailingList.URL
import Control.Monad.Trans         (liftIO)
import Control.Lens                ((^.))
import Data.Maybe                  (fromMaybe, maybe)
import Data.Monoid                 (mempty)
import Data.Text                   (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as L
import Language.Haskell.HSX.QQ     (hsx)
import Happstack.Server            (Response, seeOther, toResponse, notFound)
import HSP
import System.Directory            (doesFileExist)
import Text.Html.Email.Validate (isValidEmail)
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transformEitherM, transform, mapView)
import qualified Text.Reform.Generalized  as G
import Text.Reform.Happstack (reform)
import Text.Reform.HSP.Text (form, inputEmail, inputText, setAttrs, label, labelText, inputSubmit, errorList, textarea, fieldset)
import Web.Routes (showURL)

sendMessage :: MailingListURL
            -> MessageId
            -> MailingListM Response
sendMessage here mid =
  do template "send message" () $
      do mMsg <- query (MessageById mid)
         case mMsg of
           Nothing ->
             do html <- [hsx| <p>Message <% show $ mid ^. unMessageId %> not found.</p> |]
                notFound $ html
           (Just msg) ->
             [hsx| <div>
                    <h1>Send Message <% show $ mid ^. unMessageId %></h1>
                    <h2>Preview</h2>
                    <div>From: <%msg ^. msgFrom ^. unEmail %></div>
                    <div>Subject: <%msg ^. msgSubject %></div>
                    <pre><%msg ^. msgBody %></pre>
                   </div>
                 |]
{-
  do template "send mailing" () $
       [hsx| <%>
         <h1>Send Mailing</h1>
         <% reform (form here) "ep" sendTheMessage Nothing (sendMailingForm) %>
       </%> |]
       where
         updateMailingListSettings :: (Maybe FilePath, Email, Message) -> MailingListM Response
         updateMailingListSettings (sendmailPath, email, message) =
           do update (SetMailingListSettings sendmailPath (Just email) (Just message))
              hereURL <- showURL here
              seeOther hereURL (toResponse ())
  

sendMailingForm :: Maybe Message -> MailingListForm (Maybe FilePath, Email, Message)
sendMailingForm mMessage =
  let message = fromMaybe (optInConfirmMsg (fromMaybe (Email mempty) mEmail)) mMessage
  in
    divHorizontal $
      fieldset $
        errorList ++>
          ( (toSettings (message ^. msgId))
                 <$> errorList ++>
                     (divControlGroup (label' "sendmail path"     ++>
                      (divControls $ inputText (T.pack $ fromMaybe "" mSendmail)
                           `setAttrs` [("class" := "input-xxlarge")  :: Attr Text Text]
                           `transformEitherM`
                         (\p -> if T.null p
                                   then pure (Right Nothing)
                                   else do let fp = T.unpack p
                                           b <- liftIO $ doesFileExist fp
                                           if b
                                              then pure (Right (Just fp))
                                              else pure (Left SendmailNotFound)
                                 ))))
                 <*> errorList ++>
                      (divControlGroup (label' "From:"     ++>
                        (divControls $ inputEmail (message ^. msgFrom ^. unEmail )
                           `setAttrs` [("class" := "input-xxlarge")  :: Attr Text Text]
                            `transformEither` validEmail)))
                 <*> errorList ++>
                       (divControlGroup (label' "Subject: " ++>
                         (divControls $ inputText (message ^. msgSubject)
                           `setAttrs` [("class" := "input-xxlarge")  :: Attr Text Text]
                            `transformEither` subjectRequired)))
                 <*> errorList ++>
                       (divControlGroup (label' "Body:"     ++>
                          (divControls $ textarea 80 25 (message ^. msgBody)
                              `setAttrs` [("class" := "input-xxlarge")  :: Attr Text Text]
                               `transformEither` linkRequired)))
                 <*  (divControlGroup (divControls $ (inputSubmit (T.pack "Update") `setAttrs`[("class" := "btn") :: Attr Text Text])))
        )
     
    where
      label' :: L.Text -> MailingListForm ()
      label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr L.Text L.Text])
      divHorizontal   = mapView (\xml -> [ [hsx| <div class="form-horizontal"><% xml %></div>|]])
      divControlGroup = mapView (\xml -> [ [hsx| <div class="control-group"><% xml %></div> |]])
      divControls     = mapView (\xml -> [ [hsx| <div class="controls"><% xml %></div> |]])
      validEmail :: T.Text -> Either MailingListFormError Email
      validEmail email =
         if isValidEmail email
         then (Right (Email email))
         else (Left InvalidEmail)
      subjectRequired :: Text -> Either MailingListFormError Text
      subjectRequired subj
        | T.null subj = Left MissingSubject
        | otherwise = Right subj

      linkRequired :: T.Text -> Either MailingListFormError Text
      linkRequired bdy =
          case T.breakOn "$link$" bdy of
            (before, after)
                 | T.null after -> Left MissingLink
                 | otherwise -> Right bdy

      toSettings :: MessageId -> Maybe FilePath -> Email -> Text -> T.Text -> (Maybe FilePath, Email, Message)
      toSettings mid sendmailPath email subject bdy =
                     ( sendmailPath
                     , email
                     , Message { _msgId = mid
                               , _msgFrom = email
                               , _msgSubject = subject
                               , _msgBody = bdy
                               })

-}
