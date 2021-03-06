{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind -fprint-explicit-kinds #-}

module App where

import           Control.Lens ((^.),view)
import           Control.Monad (void, (<=<))
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as A (Value)
import           Data.Bool (bool)
import qualified Data.ByteString as B
import           Data.Foldable (for_)
import           Data.Function (on)
import           Data.List (lookup,sortBy)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe,mapMaybe,maybeToList)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.Core (text)
import           Reflex.Dom.Routing.Writer
import           Reflex.Dom.Routing.Nested
import           Language.Javascript.JSaddle hiding ((!!))
--
import           NLP.Semantics.Type (ARB(..))
import           SemanticParserAPI.Type (InputSentence(..)
                                        ,StatusResult(..)
                                        ,resultARBs
                                        ,resultPNGData
                                        ,resultSVGData
                                        ,resultConsoleOutput
                                        ,outputDocStructure
                                        ,outputMatchedFrames
                                        ,outputX'tree
                                        ,png_data
                                        ,svg_data
                                        ,statusNodes)
import           API
import           Servant.Reflex
import           Reflex.Active (Active(Dyn))
--
import           ARBView (arbView)
import           Console (consoleBox)
import           Sample
import           StatusView (renderStatus)


api :: Proxy API
api = Proxy

hostAddress :: Text
hostAddress = "mark"

hostPort :: Int
hostPort = 9191

httpBase :: BaseUrl
httpBase = BaseFullUrl Http hostAddress hostPort ""


restAPI :: Proxy RESTAPI
restAPI = Proxy

statusAPI :: Proxy STATUSAPI
statusAPI = Proxy


runWithLoader :: MonadWidget t m => m ()
runWithLoader = do
  pb <- delay 0 =<< getPostBuild
  rec loadingDimmer pb'
      liftJSM syncPoint
      pb' <- fmap updated $ widgetHold blank $ app <$ pb
  return ()


loadingDimmer :: MonadWidget t m => Event t () -> m ()
loadingDimmer evt =
  dimmer (def & dimmerConfig_page .~ True & action ?~
    (def & action_event ?~ (Transition Fade def <$ evt))) $
    divClass "ui huge text loader" $ text "Loading semantic-reflex docs..."


analyzeInput ::
       MonadWidget t m
    => Event t Text
    -> m (TextInput t,Event t ())
analyzeInput ev =
  input (def & inputConfig_fluid |~ True
             & inputConfig_action |?~ RightAction) $ do
    ti <- textInput $
            def & textInputConfig_placeholder |~ "Sentence..."
                & textInputConfig_value .~ SetValue "" (Just ev)
    btn <- analyzeButton
    pure (ti,btn)


analyzeButton :: (MonadWidget t m) => m (Event t ())
analyzeButton = button conf $ text "Analyze"
  where
    conf = def & buttonConfig_type .~ LinkButton & buttonConfig_color |?~ Teal


mkExampleDropdown :: (MonadWidget t m) => Dynamic t Bool -> m (Dropdown t (Maybe Text))
mkExampleDropdown goodex =
  dropdown def Nothing $ TaggedDynamic $
    let dexampleData = fmap (\case True -> goodExampleData; False -> exampleData) goodex
    in fmap (foldMap (\(t,_) -> t =: text t)) dexampleData


expandableSegments :: (MonadWidget t m) => [(Text,m ())] -> m ()
expandableSegments nws =
  segments def $ for_ nws $ \(name,widget) -> mdo
    open <- segment (def & segmentConfig_color |?~ Teal) $ do
      (e, _) <- elAttr' "div" ("style" =: "cursor: pointer") $ do
        icon' (Dyn $ bool "caret right" "caret down" <$> open) def
        text name
      toggle False $ domEvent Click e
    let mkTransition dir = Transition SlideDown $ def
          & transitionConfig_direction ?~ dir
          & transitionConfig_duration .~ 0.3
        actionConfig = def
          & action_event ?~ (mkTransition . bool Out In <$> updated open)
          & action_initialDirection .~ Out
    segment (def & action ?~ actionConfig) widget


sectionSentence ::
       forall t m.
       (SupportsServantReflex t m, MonadWidget t m) =>
       Client t m RESTAPI ()
    -> RouteWriterT t Text (RouteT t Text m) ()
sectionSentence postanalysis = do
  paragraph $ do
    text "Enter a sentence and then you will get a semantic analysis."

  goodex <- paragraph $ buttons def $ do
    goodex <- button def $ text "Good Example Only"
    allex <- button def $ text "All Examples"
    holdDyn False $ leftmost
      [ True <$ goodex, False <$ allex ]


  drpdn <- paragraph $ do
    text "Example sentences: "
    mkExampleDropdown goodex
  let drpdnevent :: Event t Text
      drpdnevent =
        let dyn0 = value drpdn
            dyn1 = fmap (>>= flip lookup exampleData) dyn0
            ev0 = updated dyn1
        in fmap (fromMaybe "") ev0

  response <- paragraph $ do
    (ti,btn) <- analyzeInput drpdnevent
    let inputsent = fmap (Right . InputSentence) (value ti)
    lift $ lift $ fmapMaybe reqSuccess <$> postanalysis inputsent btn
  paragraph $ do
    let -- TODO: show all graphs, not first one
        extractPNG r = case r ^. resultPNGData of
                         [] -> ""
                         dat:_ -> dat ^. png_data
        extractSVG r = case r ^. resultSVGData of
                         [] -> ""
                         dat:_ -> dat ^. svg_data

    srcpng <- holdDyn "" (fmap extractPNG response)
    srcsvg <- holdDyn "" (fmap extractSVG response)
    arbs   <- holdDyn [] (fmap (view resultARBs) response)
    dmcout <- holdDyn Nothing $ fmap (Just . view resultConsoleOutput) response

    dyn . flip fmap dmcout $ \mcout ->
      case mcout of
        Nothing -> blank
        Just cout -> do
          expandableSegments
            [ ("X'tree"       , consoleBox (constDyn (cout^.outputX'tree)))
            , ("Doc structure", consoleBox (constDyn (cout^.outputDocStructure)))
            , ("Matched Frame", consoleBox (constDyn (cout^.outputMatchedFrames)))
            ]

    let conf = def & imageConfig_shape |?~ Rounded
                   & imageConfig_size |~ Just Massive
                   & style |~ Style "overflow: hidden"
    paragraph $
      image conf (Right (img (Dyn srcpng) def))
    paragraph $
      image conf (Right (img (Dyn srcsvg) def))
    void . dyn $ fmap (mapM_ arbView) arbs



sectionReuters ::
       forall t m.
       (SupportsServantReflex t m, MonadWidget t m) =>
       RouteWriterT t Text (RouteT t Text m) ()
sectionReuters = do
  paragraph $ do
    text "Reuters section will be here."


-- TODO: remove this. This should be a part of orchestration dashboard.
sectionStatus ::
      forall t m.
       (SupportsServantReflex t m, MonadWidget t m, Monad m) =>
       Client t m STATUSAPI ()
    -> RouteWriterT t Text (RouteT t Text m) ()
sectionStatus statusCheck = do
  ebtn <- paragraph $ button def $ text "test"
  e <- count ebtn
  paragraph $ do
    text "Clicked"
    display e
  paragraph $ do
    ws <-jsonWebSocket
            ("ws://" <> hostAddress <> ":" <> T.pack (show hostPort) <> "/stream")
            (def :: WebSocketConfig t Text)
    let emstatus = _webSocket_recv ws
    void $ widgetHold blank $
      flip fmap emstatus $ \mstatus ->
        case mstatus of
          Nothing -> blank
          Just status ->
            renderStatus $ sortBy (compare `on` fst) $ view statusNodes status
    pure ()

pages :: (MonadWidget t m) => Dynamic t Int -> [m ()] -> m ()
pages dmode ws = do
  let onoff n = fmap (\m -> if m == n then Style "display: block" else Style "padding: 0; display: none")
  for_ (zip [0..] ws) $ \(i,w) ->
    ui "div" (def & style .~ Dyn (onoff i dmode)) w



app :: forall t m. (SupportsServantReflex t m, MonadWidget t m) => m ()
app =
  runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ mdo
    let postanalysis = client
                         restAPI
                         (Proxy :: Proxy m)
                         (Proxy :: Proxy ())
                         (constDyn httpBase)
    let statusCheck  = client
                         statusAPI
                         (Proxy :: Proxy m)
                         (Proxy :: Proxy ())
                         (constDyn httpBase)
    let mainConfig =  def
            & elConfigAttributes |~ ("id" =: "main")
            & elConfigClasses |~ "ui container"

    -- Header

    segment (def & attrs |~ ("id" =: "masthead") & segmentConfig_vertical |~ True) $
      divClass "ui container" $ do
        let conf = def
              & headerConfig_preContent ?~ semanticLogo
              & style |~ Style "cursor: pointer"
        (e, _) <- pageHeader' H1 conf $ do
          text "UpHere Semantic Parser"
          subHeader $ text "Semantic Parser with Reuters article analysis"
        tellRoute $ [] <$ domEvent Click e

    -- Main

    ui "div" mainConfig $ do

      -- Menu
      let s = Style "overflow: auto"
      rail RightRail (def & railConfig_dividing |~ True & style |~ s) $ sticky def $ do
        void . menu (def & menuConfig_vertical |~ True & menuConfig_secondary |~ True) $ do

          (e1, _) <- menuItem' (def & menuItemConfig_disabled |~ True) $
            text "Sentence analysis"
          tellRoute $ ["sentence"] <$ domEvent Click e1
          (e2, _) <- menuItem' (def & menuItemConfig_disabled |~ True) $
            text "Reuters Archive"
          tellRoute $ ["reuters"] <$ domEvent Click e2
          (e3, _) <- menuItem' (def & menuItemConfig_disabled |~ True) $
            text "Worker Status"
          tellRoute $ ["status"] <$ domEvent Click e3


        divider $ def & dividerConfig_hidden |~ True

      emode <- withRoute $ \route -> do
        case route of
          Just "sentence" -> pure 0
          Just "reuters"  -> pure 1
          Just "status"    -> pure 2
          _               -> pure 0


      dmode <- holdDyn 0 emode

      pages dmode [ sectionSentence postanalysis
                  , sectionReuters
                  , sectionStatus statusCheck
                  ]

    -- Footer
    segment (def & segmentConfig_vertical |~ True
                & style |~ Style "padding: 0") blank
    segment (def & segmentConfig_vertical |~ True
                & segmentConfig_aligned |?~ CenterAligned) $ do
      text "UpHere, Inc. copyright reserved"
      divider $ def & dividerConfig_hidden |~ True
      text $ "Animal icons courtesy of "
      let url = "https://www.creativetail.com/40-free-flat-animal-icons/"
      hyperlink url $ text "Creative Tail"


semanticLogo :: MonadWidget t m => m ()
semanticLogo = image (def & imageConfig_shape |?~ Rounded) $ Left $ Img url def
  where url = "https://semantic-ui.com/images/logo.png"

main :: JSM ()
main = mainWidget runWithLoader
