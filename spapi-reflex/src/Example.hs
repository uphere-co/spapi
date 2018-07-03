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

module Example where

import           Control.Lens ((^.))
import           Control.Monad (void, (<=<))
import           Control.Monad.Trans.Class (lift)
import           Data.Bool (bool)
import           Data.Foldable (for_)
import           Data.List (lookup)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe,mapMaybe,maybeToList)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.Core (text)
import           Reflex.Dom.Routing.Writer
import           Reflex.Dom.Routing.Nested
import           Language.Javascript.JSaddle hiding ((!!))


import           Example.QQ
import           Example.Common

import           Example.Section.Buttons (buttonSection)
import           Example.Section.Checkbox (checkboxes)
import           Example.Section.Dimmer (dimmers)
import           Example.Section.Divider (dividers)
import           Example.Section.Dropdown (dropdowns)
import           Example.Section.Flag (flags)
import           Example.Section.Header
import           Example.Section.Icon (iconSection)
import           Example.Section.Input (inputs)
import           Example.Section.Label (labels)
import           Example.Section.List (lists)
import           Example.Section.Message (messages)
import           Example.Section.Progress (progressSection)
import           Example.Section.RadioGroup (radioGroups)
import           Example.Section.Transition (transitions)
--
import           NLP.Semantics.Type (ARB(..))
import           SemanticParserAPI.Type (InputSentence(..)
                                        ,resultARBs
                                        ,resultPNGData
                                        ,resultSVGData
                                        ,resultOutputText
                                        ,png_data
                                        ,svg_data)
--
import           ARBView
import           Sample

import           API
import           Servant.Reflex

import           Reflex.Active (Active(Dyn))



api :: Proxy API
api = Proxy


restAPI :: Proxy RESTAPI
restAPI = Proxy

data Category t m = Category
  { categoryName :: Text
  , categoryItems :: [(Text, Status, Maybe (Section t m))]
  }
data Status = Implemented | PartiallyImplemented | NotImplemented deriving Show

-- | A log of the implementation progress
progressTable :: MonadWidget t m => [Category t m]
progressTable =
  [ Category "Elements"
    [ ("Button", Implemented, Just buttonSection)
    , ("Container", PartiallyImplemented, Nothing)
    , ("Divider", Implemented, Just dividers)
    , ("Flag", Implemented, Just flags)
    , ("Header", Implemented, Just headers)
    , ("Icon", Implemented, Just iconSection)
    , ("Image", Implemented, Nothing)
    , ("Input", Implemented, Just inputs)
    , ("Label", Implemented, Just labels)
    , ("List", Implemented, Just lists)
    , ("Loader", NotImplemented, Nothing)
    , ("Rail", Implemented, Nothing)
    , ("Reveal", NotImplemented, Nothing)
    , ("Segment", Implemented, Nothing)
    , ("Step", NotImplemented, Nothing)
    ]
  , Category "Collections"
    [ ("Breadcrumb", NotImplemented, Nothing)
    , ("Form", PartiallyImplemented, Nothing)
    , ("Grid", NotImplemented, Nothing)
    , ("Menu", NotImplemented, Nothing)
    , ("Message", Implemented, Just messages)
    , ("Table", PartiallyImplemented, Nothing)
    ]
  , Category "Views"
    [ ("Advertisement", NotImplemented, Nothing)
    , ("Card", NotImplemented, Nothing)
    , ("Comment", NotImplemented, Nothing)
    , ("Feed", NotImplemented, Nothing)
    , ("Item", NotImplemented, Nothing)
    , ("Statistic", NotImplemented, Nothing)
    ]
  , Category "Modules"
    [ ("Accordion", NotImplemented, Nothing)
    , ("Checkbox", Implemented, Just checkboxes)
    , ("Dimmer", Implemented, Just dimmers)
    , ("Dropdown", PartiallyImplemented, Just dropdowns)
    , ("Embed", NotImplemented, Nothing)
    , ("Modal", NotImplemented, Nothing)
    , ("Nag", NotImplemented, Nothing)
    , ("Popup", NotImplemented, Nothing)
    , ("Progress", Implemented, Just progressSection)
    , ("Rating", NotImplemented, Nothing)
    , ("Search", NotImplemented, Nothing)
    , ("Shape", NotImplemented, Nothing)
    , ("Sidebar", NotImplemented, Nothing)
    , ("Sticky", PartiallyImplemented, Nothing)
    , ("Tab", NotImplemented, Nothing)
    , ("Transition", Implemented, Just transitions)
    ]
  , Category "Behaviors"
    [ ("API", NotImplemented, Nothing)
    , ("Form Validation", NotImplemented, Nothing)
    , ("Visibility", NotImplemented, Nothing)
    ]
  ]

progressProgress :: forall t m. MonadWidget t m => m (Progress t m)
progressProgress = progress (pure $ Range 0 vMax) (pure v) $ def
  & progressConfig_color |?~ Teal
  & progressConfig_bar ?~ PercentageBar
  where
    categories = progressTable :: [Category t m]
    v = sum $ concatMap (fmap (\(_,i,_) -> implementedNum i) . categoryItems) categories
    vMax = sum $ map ((*2) . length . categoryItems) categories
    implementedNum = \case
      NotImplemented -> 0
      PartiallyImplemented -> 1
      Implemented -> 2

intro :: forall t m. (RouteWriter t Text m, MonadWidget t m) => Section t m
intro = Section "Introduction" blank $ do
  paragraph $ do
    text "This library aims to provide a type safe Haskell wrapper around Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It is currently in early development and started as a fork of the "
    hyperlink "https://github.com/reflex-frp/reflex-dom-semui" $
      text "reflex-dom-semui"
    text " library, although it has since been completely rewritten to remove dependencies on external JavaScript."

  message (def & messageConfig_type |?~ InfoMessage) $
    paragraph $ do
      icon "announcement" def
      text "The implementation of this library does not depend on the Semantic UI or jQuery JavaScript libraries."

  paragraph $ text "This page serves to provide an example of the library and components in use. Examples are shown along with the code that generated them."

  pageHeader H3 def $ text "Progress"

  void progressProgress

  -- Progress chart
  segments def $ for_ (progressTable @t @m) $ \(Category name items) -> mdo

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

    table (def & tableConfig_attached |?~ Attached & action ?~ actionConfig) $ do
      thead $ tr $ do
        th $ text "Feature"
        th $ text "Status"
      tbody $ for_ items $ \(item, status, mWidget) -> tr $ do
        td $ case mWidget of
          Nothing -> text item
          Just _ -> hyperlink ("#" <> toId item) $ text item
        case status of
          Implemented -> elClass "td" "positive" $ text "Implemented"
          NotImplemented -> elClass "td" "negative" $ text "Not implemented"
          PartiallyImplemented -> elClass "td" "warning" $ text "Partially implemented"

  pageHeader H3 def $ text "Notes"

  paragraph $ text "For the common use case of config values to 'pure value', there are lenses:"
  paragraph $ do
    hscode $(printDefinition oneline id '(|?~))
    hscode $(printDefinition oneline id '(|~))

  paragraph $ text "In some cases (e.g. dropdowns) we want to write one function which optimises the common case of having a static list of items. For this, see the type:"
  paragraph $ do
    hscode $(printDefinition id id ''ActiveType)
    hscode $(printDefinition id id ''TaggedActive)
  paragraph $ text "This is used in the dropdown implementation."

-- | Convert a component name to a css id string
toId :: Text -> Text
toId = T.intercalate "-" . T.words . T.toLower

main :: JSM ()
main = mainWidget runWithLoader


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



mkExampleDropdown :: (MonadWidget t m) => m (Dropdown t (Maybe Text))
mkExampleDropdown =
  dropdown def Nothing $ TaggedStatic $
    foldMap (\(t,_) -> t =: text t) exampleData -- (zip [1..] exampleData)

arbView :: (MonadWidget t m) => ARB -> m ()
arbView arb =
  -- paragraph $ do
    elClass "div" "box_analytics" $ do
      elClass "div" "side_l" $ do
        elClass "div" "in_box ty01" $ do
          el "dl" $ do
            el "dt" $ do
              elClass "span" "tit" $ do
                let blocks = formatARB arb
                -- mapM_ (paragraph . text . T.pack . show) blocks
                mapM_ arbBlock blocks
                -- text (T.pack (show arb))
                -- text "abc"
                -- display arb -- (fmap show arbs)


arbBlock :: (MonadWidget t m) => ARBBlock -> m ()
arbBlock (ARBBlock p t as) =
   elClass "span" "framenet" $
     el "div" $ do
       elClass "span" "prep" $
         text p
       el "span" $
         text t
       mapM_ arbAnnot as

arbAnnot :: (MonadWidget t m) => ARBAnnot -> m ()
arbAnnot (ARBAnnot t a) = elClass "div" t $ text a

sectionSentence ::
       forall t m.
       (SupportsServantReflex t m, MonadWidget t m) =>
       Client t m RESTAPI ()
    -> RouteWriterT t Text (RouteT t Text m) ()
sectionSentence postanalysis = do
  paragraph $ do
    text "Enter a sentence and then you will get a semantic analysis."
  drpdn <- paragraph $ do
    text "Example sentences: "
    mkExampleDropdown
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
    arbs <- holdDyn [] (fmap (^.resultARBs) response)
    otxt <- holdDyn "" (fmap (^.resultOutputText) response)
    el "pre" $
      el "code" $
        dynText otxt

    img (Dyn srcpng) def
    img (Dyn srcsvg) def
    void . dyn $ fmap (mapM_ arbView) arbs



sectionReuters ::
       forall t m.
       (SupportsServantReflex t m, MonadWidget t m) =>
       RouteWriterT t Text (RouteT t Text m) ()
sectionReuters = do
  paragraph $ do
    text "Reuters section will be here."


app :: forall t m. (SupportsServantReflex t m, MonadWidget t m) => m ()
app =
  runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ mdo
    let postanalysis = client
                         restAPI
                         (Proxy :: Proxy m)
                         (Proxy :: Proxy ())
                         (constDyn (BasePath "/"))

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
          -- display =<< holdDyn "" ("clicked" <$ (domEvent Click e))
          tellRoute $ ["sentence"] <$ domEvent Click e1
          (e2, _) <- menuItem' (def & menuItemConfig_disabled |~ True) $
            text "Reuters Archive"
          tellRoute $ ["reuters"] <$ domEvent Click e2

        divider $ def & dividerConfig_hidden |~ True

      withRoute $ \route -> do
        case route of
          Nothing         -> sectionSentence postanalysis
          Just "sentence" -> sectionSentence postanalysis
          Just "reuters"  -> sectionReuters

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
