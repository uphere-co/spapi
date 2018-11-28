{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Text (Text)

import           Servant.API

import           SemanticParserAPI.Type (APIResult,InputSentence,StatusResult)

type API =      "servant" :> Raw
           :<|> RESTAPI
           :<|> STATUSAPI
           --  :<|> CORENLPAPI

type RESTAPI = "analysis" :> ReqBody '[JSON] InputSentence :> Post '[JSON] APIResult

type STATUSAPI = "status" :> Get '[JSON] StatusResult

-- type CORENLPAPI = "corenlp" :> ReqBody '[JSON] Text :> Post '[JSON] Text
