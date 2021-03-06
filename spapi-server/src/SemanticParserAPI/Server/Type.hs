{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticParserAPI.Server.Type where

import           Data.Aeson
                 ( FromJSON(..)
                 , ToJSON(..)
                 , defaultOptions
                 , fieldLabelModifier
                 , genericParseJSON
                 , genericToJSON
                 )
import           Data.Char         ( toLower )
import           Data.Semigroup    ( (<>) )
import           Data.Text         ( Text )
import           GHC.Generics      ( Generic )
------
import           CloudHaskell.Type ( RenderError(..) )

data SPAPIServerError = SPAPIServerConfigError Text

instance RenderError SPAPIServerError where
  renderError (SPAPIServerConfigError txt) =
    "Configuration error: " <> txt


data SPAPIConfig = SPAPIConfig {
                     spapiStaticDir :: FilePath
                   , spapiPort      :: Int
                   }
                 deriving (Show,Eq,Ord,Generic)

instance FromJSON SPAPIConfig where
  parseJSON = genericParseJSON
                defaultOptions {
                  fieldLabelModifier = \name ->
                    case drop 5 name of
                      x : xs -> toLower x : xs
                      xs -> xs
                }


instance ToJSON SPAPIConfig where
  toJSON = genericToJSON
             defaultOptions {
               fieldLabelModifier = \name ->
                 case drop 5 name of
                   x : xs -> toLower x : xs
                   xs -> xs
             }
