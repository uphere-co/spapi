{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex.Dom


main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec el "h2" $ text "Counter As a FOLD"
      counts <- foldDyn (+) (0 :: Int) $
                  leftmost [ 1 <$ evIncr, -1 <$ evDecr ]
      el "div" $ display counts
      evIncr <- button "Inc"
      evDecr <- button "Dec"
  pure ()



