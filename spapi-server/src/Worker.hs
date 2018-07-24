{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Worker where

import           Control.Lens                        ((^.),(^..))
import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (liftIO)
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as B
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                          (fromMaybe)
import           Data.Proxy                          (Proxy(..))
import           Data.Semigroup                      ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as TL
import           Servant.Server                      (Handler)
import           System.Directory                    (getCurrentDirectory
                                                     ,setCurrentDirectory
                                                     ,getTemporaryDirectory
                                                     ,removeFile)
import           System.FilePath                     ((</>),(<.>))
import           System.Process                      (readProcess)
-- language-engine layer
import           FrameNet.Query.Frame                (FrameDB,frameDB)
import qualified FrameNet.Type.Definition      as F
import           FrameNet.Type.Frame                 (frame_definition)
import           Lexicon.Type                        (RoleInstance)
import           NLP.Semantics.Type                  (MeaningRoleContent(..),MeaningTree(..)
                                                     ,mt_frame,mt_arguments,mt_subordinates
                                                     ,mr_content,po_main)
import           SRL.Analyze.ARB                     (mkARB)
import           SRL.Analyze.Format                  (dotMeaningGraph)
import           SRL.Analyze.Format.OGDF             (mkOGDFSVG)
import           SRL.Analyze.MeaningTree             (mkMeaningTree)
import           SRL.Analyze.Type                    (MeaningGraph
                                                     ,outputDocStructure,outputMatchedFrames
                                                     ,outputX'tree)
-- compute-pipeline layer
import           CloudHaskell.QueryQueue             (QQVar,singleQuery)
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..)
                                                     ,StatusQuery(..),StatusResult(..)
                                                     ,ResultSentence(..))
import           SemanticParserAPI.Type              (InputSentence(..),PNGData(..),APIResult(..)
                                                     ,DefRoot(..),CContent(..),EContent(..)
                                                     ,SVGData(..)
                                                     )
import qualified SemanticParserAPI.Type        as S  (ConsoleOutput(ConsoleOutput))
-- spapi layer
import           API

api :: Proxy API
api = Proxy


withTempFile :: (String,String) -> Int -> (FilePath -> IO a) -> IO a
withTempFile (base,ext) i action = do
  tmpdir <- getTemporaryDirectory
  let file = tmpdir </> base ++ show i <.> ext
  r <- action file
  removeFile file
  pure r


uriEncode :: B.ByteString -> B.ByteString -> Text
uriEncode mimetype bstr =
  TE.decodeUtf8 ("data:" <> mimetype <> ";base64," <> B64.encode bstr)

createDotGraph :: MeaningGraph -> IO PNGData
createDotGraph mg = do
  let dotstr = dotMeaningGraph Nothing mg
  cdir <- getCurrentDirectory
  tdir <- getTemporaryDirectory
  setCurrentDirectory tdir
  TIO.writeFile "test.dot" dotstr
  -- TODO remove explicit path
  void (readProcess "/nix/store/hxwdxsg6w79cnj2slkhk3bs8fx6nvdyk-graphviz-2.40.1/bin/dot" ["-Tpng","test.dot","-otest.png"] "")
  -- TODO use temporary name and remove
  bstr <- B.readFile "test.png"
  setCurrentDirectory cdir
  let pngdata = PNGData (uriEncode "image/png" bstr)
  pure pngdata


createOGDFSVG :: (Int,MeaningGraph) -> IO SVGData
createOGDFSVG (i,mg) =
  withTempFile ("test","svg") i $ \file -> do
    mkOGDFSVG file mg
    bstr <- B.readFile file
    let svgdata = SVGData (uriEncode "image/svg+xml" bstr)
    pure svgdata


allFrames :: MeaningTree -> [Text]
allFrames mt = let frm0 = mt^.mt_frame
               in frm0 : (ys ++ zs)
  where
    xs = mt^..mt_arguments.traverse.mr_content
    ys = concatMap (f . (^.po_main)) xs
    zs = concatMap allFrames (mt^.mt_subordinates)
    --
    f (SubFrame x) = allFrames x
    f (Modifier _ subs) = concatMap allFrames subs
    f _ = []


convertDefRoot :: F.DefRoot -> DefRoot
convertDefRoot (F.DefRoot lst) = DefRoot (map convertCContent lst)
  where
    convertCContent (F.CTEXT txt) = CTEXT txt
    convertCContent (F.CFEN txt)  = CFEN txt
    convertCContent (F.CEX xs)    = CEX (map convertEContent xs)
    convertCContent F.CRET        = CRET ()
    --
    convertEContent (F.ETEXT txt) = ETEXT txt
    convertEContent F.ERET        = ERET ()
    convertEContent (F.EM txt)    = EM txt
    convertEContent (F.EFEX txt)  = EFEX txt

deriving instance Show F.DefRoot


mkFrameNetData :: FrameDB -> Text -> (Text,DefRoot)
mkFrameNetData framemap fname = fromMaybe (fname,DefRoot []) $ do
  frm <- HM.lookup fname (framemap^.frameDB)
  let txt = frm^.frame_definition
      defroot0 = F.p_defRoot (TL.fromStrict txt)
  return (fname,convertDefRoot defroot0)

postAnalysis ::
     FrameDB
  -> [RoleInstance]
  -> QQVar ComputeQuery ComputeResult
  -> InputSentence
  -> Handler APIResult
postAnalysis framedb rolemap qqvar (InputSentence sent) = do
  CR_Sentence (ResultSentence _ tokss mgs cout) <- liftIO (singleQuery qqvar (CQ_Sentence sent))
  dots <- liftIO $ mapM createDotGraph mgs
  svgs <- liftIO $ mapM createOGDFSVG (zip [1..] mgs)
  let mts = concatMap (mkMeaningTree rolemap) mgs
      arbs = concatMap (mkARB rolemap) mgs
      fns = map (mkFrameNetData framedb) (concatMap allFrames mts)
      cout' = S.ConsoleOutput (cout^.outputX'tree) (cout^.outputDocStructure) (cout^.outputMatchedFrames)
  pure (APIResult tokss mts arbs dots svgs fns cout')


batchTest :: QQVar StatusQuery StatusResult -> Handler Text
batchTest qqvar = T.pack . show <$> liftIO (singleQuery qqvar SQ)
