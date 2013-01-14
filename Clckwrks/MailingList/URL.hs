{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.MailingList.URL where

import Data.Data           (Data, Typeable)
import Web.Routes.TH       (derivePathInfo)

data MailingListAdminURL
    = ViewSubscribers
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''MailingListAdminURL)

data MailingListURL
    = Subscribe
    | MailingListAdmin MailingListAdminURL
    | MailingListData String
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''MailingListURL)
