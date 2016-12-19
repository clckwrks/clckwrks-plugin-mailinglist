{-# LANGUAGE FlexibleContexts, QuasiQuotes, RecordWildCards, OverloadedStrings, TypeFamilies #-}
module Clckwrks.MailingList.Page.Subscribe where

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
import Network.Mail.Mime (sendmail, sendmailCustom, renderMail')
import HSP
import Language.Haskell.HSX.QQ (hsx)
import Text.Html.Email.Validate (isValidEmail)
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transform, mapView)
import qualified Text.Reform.Generalized  as G
import Text.Reform.Happstack (reform)
import Text.Reform.HSP.Text (form, inputEmail, inputText, setAttrs, label, inputSubmit, errorList)

subscribePage :: MailingListURL -> MailingListM Response
subscribePage here =
    do mOptInConfirmMsg <- query GetOptInConfirmMessage
       case mOptInConfirmMsg of
          (Just optInConfirmMsg) ->
            template (fromString "Subscribe to Our Mailing List") ()
                 [hsx|
                  <section class="section">
                   <div class="container">
                     <h1 class="title is-1">Subscribe</h1>
                     <p>Enter your email to subscribe to our mailing list. A confirmation link will be sent to your email address. You must click on the link to complete your subscription. You can unsubscribe at anytime.</p>
                     <% reform (form here) "sub" (subscribe optInConfirmMsg) Nothing emailForm %>
                   </div>
                 </section>
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
              do url <- withAbs $ showURLParams ConfirmOptIn [("id", Just $ Text.pack $ show $ sub ^. subId ^. unSubscriberId), ("uuid", Just (UUID.toText uuid))]
                 mail <- liftIO $ renderMail' (sendStringTemplateEmail [("link", url)] message email)
                 liftIO $ Char8.putStrLn mail
                 mSendmailPath <- query GetSendmailPath
                 liftIO $ case mSendmailPath of
                           Nothing -> sendmail mail
                           (Just path) -> sendmailCustom path ["-t"] mail
                 template "Subscription Confirmation Sent!" ()
                  [hsx|
                    <p>A confirmation email has been sent to your email address. You must click on the link in the email to confirm your subscription.</p>
                  |]

type MailingListFormT m = ClckFormT MailingListFormError (MailingListT m)
emailForm :: (Functor m, MonadIO m, Happstack m) => MailingListFormT m Email
emailForm =
  (formGrp (errorList ++> (label ("Email:" :: Text) `setAttrs` [("class" := "label"):: Attr Text Text]) ++>
            (p $ (inputEmail mempty `setAttrs` [("class" := "input"):: Attr Text Text]) `transformEither` email))) <*
  (formGrp (inputSubmit (pack "subscribe") `setAttrs` [("class" := "button btn btn-default"):: Attr Text Text]))

  where
    p frm = mapView (\xml -> [hsx| [<p class="control"><% xml %></p>] |]) frm
    formGrp frm = mapView (\xml -> [hsx| [<div class="form-group control"><% xml %></div>] |]) frm
    email addr =
      if isValidEmail addr
       then Right $ Email addr
       else Left InvalidEmail

