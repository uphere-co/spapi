{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)
import Servant.API

data Item = Item { itemId :: Integer
                 , itemText :: String
                 }
          deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

type API = "servant" :> Raw :<|> RESTAPI

type RESTAPI = "item" :> Get '[JSON] Item
