{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Text (Text)

import           Servant.API

import           SemanticParserAPI.Type (APIResult,InputSentence)

type API = "servant" :> Raw :<|> RESTAPI :<|> BATCHAPI

type RESTAPI = "analysis" :> ReqBody '[JSON] InputSentence :> Post '[JSON] APIResult

type BATCHAPI = "batch" :> Get '[JSON] Text
