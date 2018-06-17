{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module ARBView where

import           Control.Applicative            ((<|>))
import           Control.Lens                   ((^.),(^?),_1,_2,to,_Just,_Left,_Right,_head)
import           Control.Lens.Extras            (is)
import qualified Control.Monad.Trans.State as S
import           Data.Aeson                     (FromJSON(..),ToJSON(..),genericParseJSON,genericToJSON)
import           Data.Aeson.Types               (defaultOptions,fieldLabelModifier)
import           Data.Function                  (on)
import           Data.List                      (find,groupBy,intersperse,partition,sortBy)
import           Data.Maybe                     (fromMaybe,mapMaybe,maybeToList)
import           Data.Monoid                    ((<>),First(..))
import           Data.Text                      (Text)
import qualified Data.Text                 as T
import           GHC.Generics                   (Generic)
--
-- import           GHCJS.Marshal                  (toJSVal)
-- import           GHCJS.Types                    (JSVal)
--
import           NLP.Semantics.Type
--
-- import           Util


data AnnotType = AnnotFrame | AnnotFrameElement | AnnotParen
               deriving (Show,Eq,Ord)




frameclass :: AnnotType -> Text
frameclass AnnotFrame = "frame"
frameclass AnnotFrameElement = "frameelement"
frameclass AnnotParen = "paren"


lastItems :: [[Text]]
lastItems = [ ["Initial_value","Final_value"]
            , ["Value_1","Value_2"]
            ]

absoluteLast :: [Text]
absoluteLast = [ "Time_vector", "Event_description", "Concessive", "Conditional_occurrence", "Negative_conditional" ]


hyphen :: (Either (Int,Text) (PrepOr Text),Maybe (Int,Text,AnnotType))
hyphen = (Right (PrepOr Nothing "\8212"), Nothing)

lparen :: Maybe Text -> Maybe (Int,Text,AnnotType) -> (Either (Int,Text) (PrepOr Text),Maybe (Int,Text,AnnotType))
lparen mprep mn = (Right (PrepOr mprep "("), mn)

rparen :: (Either (Int,Text) (PrepOr Text),Maybe (Int,Text,AnnotType))
rparen = (Right (PrepOr Nothing ")"), Nothing)


fmtannot :: Int -> Int -> [(Int,Text,AnnotType)] -> [(Text,AnnotType)]
fmtannot maxn c ((n,t,b):xs) = replicate (n-1-c) ("\8203",AnnotFrameElement) ++ [(t,b)] ++ fmtannot maxn n xs
fmtannot maxn c []           = replicate (maxn-c) ("\8203",AnnotFrameElement)


annotGroup :: [(Int,(Either (Int,Text) (PrepOr Text),Maybe (Int,Text,AnnotType)))]
           -> [(Int,(Either (Int,Text) (PrepOr Text),[(Int,Text,AnnotType)]))]
annotGroup tbl = let f ( i,(Right _,mc)) = maybeToList ((i,)<$>mc)
                     f (_i,(Left (n,t),mc)) =
                       maybeToList $ do
                         x <- find (\x -> x^?_2._2._Just._1 == Just n && x^?_2._1._Right.po_main == Just t) tbl
                         let j =x^._1
                         ((j,)<$>mc)
                     tbl' = concatMap f tbl
                 in (match tbl . map (sortBy (compare `on` (^._1))) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)) tbl'
  where
    match1 lsts x = do let i = x^._1
                       ((do lst <- find (\lst -> lst^?_head._1 == Just i) lsts
                            return (i,(x^._2._1,map (^._2) lst)))
                        <|>
                        return (i,(x^._2._1,[])))
    match tbl' lsts = mapMaybe (match1 lsts) tbl'




selectFixedOrderItems :: [(Text,a)] -> ([(Text,a)],[(Text,a)])
selectFixedOrderItems xs = fromMaybe (xs,[]) (getFirst (foldMap (First . match) lastItems))
  where match set = do
          xs' <- traverse (\k -> find (\(k',_) -> k == k') xs) set
          let ks' = map fst xs'
          return (filter (\(k,_) -> not (k `elem` ks')) xs, xs')

reorderObjects :: [(Text,Either (PrepOr ARB) (PrepOr Text))]
               -> [(Text,Either (PrepOr ARB) (PrepOr Text))]
reorderObjects ys = let (xslast,xs0) = partition (\x->x^._1 `elem` absoluteLast)  ys
                        (xs1,xsfixed) = selectFixedOrderItems xs0
                        (xsmid,xsinit) = partition (^._2.to (is _Left)) xs1
                        (xsinit_0,xsinit_1) = partition (\x -> x^?_2._Right.po_prep == Just Nothing) xsinit
                    in  (xsinit_0++xsinit_1) ++ xsmid ++ xsfixed ++ xslast

mkTableARB :: Maybe (Text,ARB)
           -> PrepOr ARB
           -> S.State Int [(Either (Int,Text) (PrepOr Text),Maybe (Int,Text,AnnotType))]
mkTableARB mp (PrepOr mprep x) = do
  n <- S.get
  S.put (n+1)

  let origobjs = x^.objectB.to reorderObjects
  objss <- flip mapM origobjs $ \(fe,ey) ->
             case ey of
               Left  y -> mkTableARB (Just (fe,x)) y
               Right y -> return [(Right y,Just (n,fe,AnnotFrameElement))]
  let objs = intersperse [hyphen] objss

  return $ (case mp of
              Nothing -> []
              Just (fe,_p) -> [lparen mprep (Just (n-1,fe<>"\10230",AnnotParen))]
           ) <>
           subjtxt n mp x <>
           -- negation as a preposition for verb.
           [(Right ((if x^.isNegated then PrepOr (Just "NOT") else noprep) (T.toUpper (x^.predicateR._2))),Just (n,x^.predicateR._1,AnnotFrame))] <>
           (if ((not.null) objs)
            then [hyphen] <> concat objs
            else []) <>
           (if is _Just mp then [rparen] else [])
 where
  subjtxt n mp' z =
    case mp' of
      Nothing    -> [(Right (noprep (z^.subjectA._2)),Just (n,z^.subjectA._1,AnnotFrameElement)), hyphen]
      Just (_,p) -> if z^.subjectA._2 == p^.subjectA._2
                    then [ (Left (n-1, z^.subjectA._2), Just (n,z^.subjectA._1,AnnotFrameElement)) ]
                    else [ (Right (noprep (z^.subjectA._2)), Just (n,z^.subjectA._1,AnnotFrameElement))
                         , hyphen ]

data ARBBlock = ARBBlock { _arb_prep :: Text
                         , _arb_text :: Text
                         , _arb_annots :: [ARBAnnot]
                         }
              deriving (Show,Generic)

instance FromJSON ARBBlock where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

instance ToJSON ARBBlock where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }

data ARBAnnot = ARBAnnot { _ann_type :: Text
                         , _ann_annot :: Text }
              deriving (Show,Generic)


instance FromJSON ARBAnnot where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

instance ToJSON ARBAnnot where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }


mkARBBlock :: ((Text,Text),[(Text,AnnotType)]) -> ARBBlock
mkARBBlock ((p,t),anns) = ARBBlock p t (map (\(ann,typ) -> ARBAnnot (frameclass typ) ann) anns)


formatARB :: ARB -> [ARBBlock]
formatARB x = let tbl = zip [1..] (S.evalState (mkTableARB Nothing (PrepOr Nothing x)) 1)
                  maxn = let ns = mapMaybe (^?_2._2._Just._1) tbl in if (not.null) ns then  maximum ns else 0
                  fmt t = either (const ("","")) (\(PrepOr mp t') -> (maybe "" T.toUpper mp,t')) t
                  tbl1 = annotGroup tbl
                  txts :: [((Text,Text),[(Text,AnnotType)])]
                  txts = map (\(_i,(t,lst)) -> (fmt t,fmtannot maxn 0 lst)) tbl1
              in map mkARBBlock txts



{- 
domARBBlock :: ARBBlock -> IO JSVal
domARBBlock (ARBBlock p t as) = do
  v <- toJSVal (toJSON as)
  n <- createElement "arb-block"
  n ^. jss "prep" p
  n ^. jss "text" t
  n ^. jss "annots" v
  return n


domARBView :: JSVal -> [ARBBlock] -> IO ()
domARBView this blks = do
  ns <- mapM domARBBlock blks

  -- for the time being
  x <- this^.js "shadowRoot".jsf "querySelector" ("#content" :: Text)
  js_clear_dom_children x

  x `appendChildren` ns
-}
