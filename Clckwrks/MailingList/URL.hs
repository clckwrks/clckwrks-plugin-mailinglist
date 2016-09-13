{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.MailingList.URL where

import Data.Data           (Data, Typeable)
import Web.Routes.TH       (derivePathInfo)
import Clckwrks.MailingList.Types (MessageId(..))

data MailingListAdminURL
    = EditMLSettings
    | ViewSubscribers
    | SendMessage MessageId
    | NewMessage
    | EditMessage MessageId
    | ViewMessages
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''MailingListAdminURL)

-- http://www.list-unsubscribe.com/
data MailingListURL
    = Subscribe
    | UnsubscribeLink
    | ConfirmOptIn
    | MailingListAdmin MailingListAdminURL
    | MailingListData String
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''MailingListURL)
