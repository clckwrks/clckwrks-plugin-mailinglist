{-# LANGUAGE FlexibleContexts, QuasiQuotes, RecordWildCards, OverloadedStrings, TypeFamilies #-}
module Clckwrks.MailingList.Page.ConfirmOptIn where

import Control.Applicative (optional)
import Control.Lens  ((^.))
import Control.Lens.At (at)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (get)
import Clckwrks
import Clckwrks.MailingList.Acid
import Clckwrks.MailingList.Monad
import Clckwrks.MailingList.Page.Template (template)
import Clckwrks.MailingList.Types
import Clckwrks.MailingList.URL
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.String (fromString)
import Data.Monoid (mempty)
import Data.Maybe  (fromJust)
import qualified Data.Set as Set
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Data.UUID.V4      (nextRandom)
import Network.Mail.Mime (renderSendMail, renderMail')
import Happstack.Server (look)
import HSP
import Language.Haskell.HSX.QQ (hsx)
import Text.Html.Email.Validate (isValidEmail)
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transform, mapView)
import qualified Text.Reform.Generalized  as G
import Text.Reform.Happstack (reform)
import Text.Reform.HSP.Text (form, inputText, setAttrs, label, inputSubmit, errorList)


-- |FIXME: GET requests are not supposed to modify the state
-- |FIXME: when subscription fails but they are already subscribed
confirmOptInPage :: MailingListM Response
confirmOptInPage =
    do mId <- optional $ look "id"
       mUUID <- optional $ look "uuid"
       case (mId, mUUID) of
        (Just idText, Just uuidStr) ->
          case reads idText of
           [(n, [])] ->
             case UUID.fromString uuidStr of
              (Just uuid) ->
                do now <- liftIO $ getCurrentTime
                   r <- update (VerifyOptIn now (SubscriberId n) uuid)
                   case r of
                    SubscriptionConfirmed -> validOptIn
                    InvalidConfirmation -> invalidOptIn
                    AlreadySubscribed -> alreadySubscribed
              _ -> invalidOptIn
        _ -> invalidOptIn
  where
    validOptIn =
      template (fromString "Subscription Confirmed!") ()
        [hsx|
          <div>
           <h1>Subscription Confirmed</h1>
           <p>Your subscription has been successfully confirmed.</p>
          </div>
        |]
    invalidOptIn =
      template (fromString "Invalid Confirmation") ()
        [hsx|
          <div>
           <h1>Invalid Confirmation</h1>
           <p>We were unable to confirm your subscription.</p>
          </div>
        |]
    alreadySubscribed =
      template (fromString "Already Subscribed") ()
        [hsx|
          <div>
           <h1>Already Subscribed</h1>
           <p>You have already confirmed your subscription.</p>
          </div>
        |]

{-          
          do r <- udpate ConfirmOptIn email uuid
        ->a       mOptInConfirmMsg <- query GetOptInConfirmMessage
       case mOptInConfirmMsg of
          (Just optInConfirmMsg) ->
            template (fromString "Subscribe to Our Mailing List") ()
                 [hsx|
                  <div>
                   <% reform (form here) "sub" (subscribe optInConfirmMsg) Nothing emailForm %>
                  </div>
                 |]
          Nothing ->
            template (fromString "Server Configuration Error") ()
                [hsx|
                 <div>
                  <h1>Server Configuration Error</h1>
                  <p>Sorry. The server administrator has not yet configuration the mailing list plugin.</p>
                 </div>
                |]
    where
      subscribe :: Message -> Email -> MailingListM Response
      subscribe message email =
        do uuid <- liftIO nextRandom
           now  <- liftIO getCurrentTime
           sub <- update (SignupSubscriber email (AwaitingConfirmation uuid) now)
           case sub ^. subStatus of
            (_, Subscribed):_ ->
              template "You are already subscribed!" ()
                [hsx|
                  <p>You are already subscribed to this mailing list.</p>
                |]
            (_, AwaitingConfirmation uuid):_ ->
              do url <- showURLParams ConfirmOptIn [("email", Just $ email^.unEmail), ("uuid", Just (UUID.toText uuid))]
                 liftIO $ Char8.putStrLn =<< renderMail' (sendStringTemplateEmail [("link", url)] message email)
                 template "Subscription Confirmation Sent!" ()
                  [hsx|
                    <p>A confirmation email has been sent to your email address. You must click on the link in the email to confirm your subscription.</p>
                  |]

emailForm :: MailingListForm Email
emailForm =
  (formGrp (errorList ++> label ("Email:" :: Text) ++> (inputText mempty `transformEither` email))) <*
  (formGrp (inputSubmit (pack "subscribe") `setAttrs` [("class" := "btn btn-default"):: Attr Text Text]))

  where
    formGrp frm = mapView (\xml -> [hsx| [<div class="form-group"><% xml %></div>] |]) frm
--    inputEmail :: (Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text text)) =>
--             (input -> Either error text)
--          -> text
--          -> Form m input error [XMLGenT x (XMLType x)] () text
    inputEmail getInput initialValue = G.input getInput inputField initialValue
     where
       inputField i a = [hsx| [<input type="email" class="form-control" id=i name=i value=a />] |]
    email addr =
      if isValidEmail addr
       then Right $ Email addr
       else Left InvalidEmail

-}
