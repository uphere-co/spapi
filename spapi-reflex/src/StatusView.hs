{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
module StatusView where

import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.Core (text)


renderNode ::
     forall t m. (MonadWidget t m) =>
     (Text,Maybe (Bool,Int))
  -> m ()
renderNode (name,status) =
  label (def & labelConfig_image |~ True) $ do
    case status of
      Nothing    -> icon "circle" $ def & iconConfig_color |?~ Red
      Just (False,n) -> do
        icon "circle" $ def & iconConfig_color |?~ Green
        text (T.pack (show n))
      Just (True,n)  -> do
        icon "circle" $ def & iconConfig_color |?~ Yellow
        text (T.pack (show n))
    text name

