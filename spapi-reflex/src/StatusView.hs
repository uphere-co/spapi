{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
module StatusView where

import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.Core (text)


renderNode
  :: forall t m. (MonadWidget t m) =>
     (Text,Maybe Bool)
  -> m ()
renderNode (name,status) =
  label (def & labelConfig_image |~ True) $ do
    case status of
      Nothing    -> icon "circle" $ def & iconConfig_color |?~ Red
      Just False -> icon "circle" $ def & iconConfig_color |?~ Green
      Just True  -> icon "circle" $ def & iconConfig_color |?~ Yellow
    text name


renderStatusItem
  :: forall t m. (MonadWidget t m) =>
     (Text,Maybe (Bool,Int))
  -> m ()
renderStatusItem (name,status) = do
  let node = renderNode (name,fmap fst status)
  listItem (def & listItemConfig_preContent ?~ node) $ do
    listHeader $ text $
      case status of
        Nothing -> "Not available"
        Just (b,_) ->
             case b of
               True -> "Now serving"
               False -> "Ready for serving"
    listDescription $ text $
      case status of
        Nothing -> ""
        Just (b,n) -> T.pack (show n) <> " tasks have been served."

renderStatus
  :: forall t m. (MonadWidget t m) =>
     [(Text,Maybe (Bool,Int))]
  -> m ()
renderStatus lst = do
  list (def & listConfig_divided |~ True & listConfig_relaxed |?~ Relaxed) $
    mapM_ renderStatusItem lst
