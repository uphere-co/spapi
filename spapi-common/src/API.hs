{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Aeson         (FromJSON(..),ToJSON(..))
import           GHC.Generics       (Generic)
import           Servant.API

import           SemanticParserAPI.Type (APIResult,InputSentence)

type API = "servant" :> Raw :<|> RESTAPI

type RESTAPI = "analysis" :> ReqBody '[JSON] InputSentence :> Post '[JSON] APIResult
