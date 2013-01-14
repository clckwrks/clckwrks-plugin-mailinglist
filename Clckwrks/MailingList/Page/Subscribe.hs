{-# LANGUAGE FlexibleContexts, RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.MailingList.Page.Subscribe where

import Control.Monad.Reader (ReaderT, ask)
import Clckwrks
import Clckwrks.MailingList.Acid
import Clckwrks.MailingList.Monad
import Clckwrks.MailingList.Types
import Clckwrks.MailingList.URL
import Clckwrks.MailingList.Page.Template (template)
import Data.String (fromString)
import Data.Monoid (mempty)
import Data.Maybe  (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Set as Set
import HSP
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transform, view)
import Text.Reform.Happstack
import Text.Reform.HSP.Text

import Text.Reform

subscribePage :: MailingListURL -> MailingListM Response
subscribePage here =
    do template (fromString "Subscribe to Our Mailing List") ()
              <%>
               <% reform (form here) "sub" subscribe Nothing emailForm %>
              </%>
    where
      subscribe :: Email -> MailingListM Response
      subscribe email =
          do template "Subscribed!" ()
                      <p>Your email has been added to our mailing list. Thank you!</p>

emailForm :: MailingListForm Email
emailForm =
  (fieldset $ ol $
       (li $ errorList ++> labelText "Email:" ++> (inputText mempty `transformEither` email))
    <* (li $ inputSubmit (pack "subscribe"))
       )
  where
    email addr =
        case Text.find (== '@') addr of
          Nothing  -> Left InvalidEmail
          (Just _) -> (Right $ Email addr)

