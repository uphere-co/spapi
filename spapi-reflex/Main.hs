{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified App

#ifndef ghcjs_HOST_OS

import Reflex.Dom.SemanticUI.Warp
import Data.ByteString
import Data.FileEmbed

#endif

main :: IO ()

#ifdef ghcjs_HOST_OS

main = App.main

#else


#endif
