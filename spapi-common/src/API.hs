{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Servant.API

import           SemanticParserAPI.Type (APIResult,InputSentence)

type API = "servant" :> Raw :<|> RESTAPI

type RESTAPI = "analysis" :> ReqBody '[JSON] InputSentence :> Post '[JSON] APIResult
