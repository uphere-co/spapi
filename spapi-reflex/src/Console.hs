{-# LANGUAGE OverloadedStrings #-}
module Console where

import           Data.Text (Text)
import           Reflex.Dom.SemanticUI


consoleBox :: (MonadWidget t m) => Dynamic t Text -> m ()
consoleBox dtxt =
  segment (def & segmentConfig_inverted |~ True
               & style |~ Style "overflow-x: scroll"
          ) $ do
    el "pre" $
      el "code" $
        dynText dtxt
