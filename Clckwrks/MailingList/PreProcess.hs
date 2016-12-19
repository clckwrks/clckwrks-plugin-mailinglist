{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.MailingList.PreProcess where

import Control.Monad.Reader            (mapReaderT)
import Control.Monad.Trans
import Control.Applicative
import Clckwrks                         (ClckT, ClckState)
import Clckwrks.MailingList.URL                (MailingListURL(..))
import Clckwrks.MailingList.Page.Subscribe      (emailForm)
import Clckwrks.MailingList.Monad
import Clckwrks.MailingList.Types              (Email)
import Clckwrks.Monad                   (mapClckT, transform, segments)
import Data.Attoparsec.Text.Lazy        (Parser, Result(..), char, choice, decimal, parse, skipMany, space, asciiCI, skipMany)
import Data.Monoid                      (mempty)
import           Data.Text              (Text, pack)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Happstack.Server                 (Happstack)
import HSP
import HSP.HTML4                        (renderAsHTML)
import Text.Reform                      (viewForm)
import Text.Reform.Happstack            (happstackViewForm, happstackView)
import Text.Reform.HSP.Text             (form)
import Web.Routes                       (showURL)

data MailingListCmd
    = SubscribeFormLink
    | SubscribeForm

parseAttr :: Text -> Parser ()
parseAttr name =
    do skipMany space
       asciiCI name
       skipMany space
       char '='
       skipMany space

parseCmd :: Parser MailingListCmd
parseCmd =
    choice [ asciiCI (pack "subscription-form-link") *> pure SubscribeFormLink
           , asciiCI (pack "subscription-form") *> pure SubscribeForm
           ]

mailingListCmd :: (MailingListURL -> [(Text, Maybe Text)] -> Text)
               -> TL.Text
               -> (forall m. (Functor m, MonadIO m, Happstack m) => MailingListT m TL.Text)
mailingListCmd mailingListShowURL txt =
    case parse (segments "mailing-list" parseCmd) txt of
      (Fail _ _ e) -> return (TL.pack e)
      (Done _ segments) ->
          do b <- transform (applyCmd mailingListShowURL) segments
             return $ B.toLazyText b


-- type MailingListForm = ClckFormT MailingListFormError MailingListM
-- type ClckFormT error m = Form m  [Input] error [XMLGenT m XML] ()
-- type MailingListM   = ClckT MailingListURL (ReaderT MailingListConfig (ServerPartT IO))
-- type MailingListT m = ClckT MailingListURL (ReaderT MailingListConfig m)
-- type MailingListForm = ClckFormT MailingListFormError MailingListM
applyCmd :: (MailingListURL -> [(Text, Maybe Text)] -> Text)
         -> MailingListCmd
         -> (forall m. (Functor m, MonadIO m, Happstack m)=> MailingListT m Builder)

applyCmd mailingListShowURL SubscribeFormLink =
     do html <- unXMLGenT $ <a href=(mailingListShowURL Subscribe [])>Join our mailing list!</a>
        return $ B.fromString $ concat $ lines $ TL.unpack $ renderAsHTML html

{-
In order for this to work, we need to be able to add CSRF cookies. However, at present the preprocessor framework only allows you to transform the body. It runs in `ClckT ClckURL IO TL.Text` not `ClckT ClckURL (ServerPartT IO) TL.Text`
-}

applyCmd mailingListShowURL SubscribeForm =
  do url <- showURL Subscribe
     view <- happstackViewForm (\hd v -> form url (("formname", "sub"):hd) v) "sub" emailForm
     html <- unXMLGenT $ sequence view
     return $ B.fromString $ concatMap (concat . lines . TL.unpack . renderAsHTML) html
{-
foo =
    do html <- happstackViewForm (form "") "sub" emailForm
       return ()
-}
