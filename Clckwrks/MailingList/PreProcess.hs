{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.MailingList.PreProcess where

import Control.Monad.Trans
import Control.Applicative
import Clckwrks                         (ClckT, ClckState)
import Clckwrks.MailingList.URL                (MailingListURL(..))
import Clckwrks.MailingList.Page.Subscribe      (emailForm)
import Clckwrks.MailingList.Monad
import Clckwrks.MailingList.Types              (Email)
import Clckwrks.Monad                   (transform, segments)
import Data.Attoparsec.Text.Lazy        (Parser, Result(..), char, choice, decimal, parse, skipMany, space, asciiCI, skipMany)
import Data.Monoid                      (mempty)
import           Data.Text              (Text, pack)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import HSP
import HSP.HTML                         (renderAsHTML)
import HSP.Identity                     (evalIdentity)
import Text.Reform.Happstack            (happstackViewForm)
import Text.Reform.HSP.Text             (form)
import Web.Routes                       (showURL)

data MailingListCmd
    = SubscribeFormLink

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
           ]

mailingListCmd :: (MailingListURL -> [(Text, Maybe Text)] -> Text)
               -> TL.Text
               -> MailingListT IO TL.Text
mailingListCmd mailingListShowURL txt =
    case parse (segments "mailing-list" parseCmd) txt of
      (Fail _ _ e) -> return (TL.pack e)
      (Done _ segments) ->
          do b <- transform (applyCmd mailingListShowURL) segments
             return $ B.toLazyText b

applyCmd :: (MailingListURL -> [(Text, Maybe Text)] -> Text)
         -> MailingListCmd
         -> MailingListT IO Builder
applyCmd mailingListShowURL SubscribeFormLink =
    do html <- unXMLGenT $ <a href=(mailingListShowURL Subscribe [])>Join our mailing list!</a>
       return $ B.fromString $ concat $ lines $ renderAsHTML html
{-
foo =
    do html <- happstackViewForm (form "") "sub" emailForm
       return ()
-}