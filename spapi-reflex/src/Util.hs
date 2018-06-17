-- {-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Util where

import           Control.Lens                     ((^.),IndexPreservingGetter)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                  (ByteString)
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.List                        (foldl')
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time.Calendar
import           Data.Time.Clock
-- import           Yesod.Routes.Class
--
-- import           Data.JSString                    (pack)
import           Data.List                        (intersperse)
-- import           GHCJS.Foreign.Callback
-- import           GHCJS.Marshal
-- import           GHCJS.Types
-- import qualified Language.Javascript.JSaddle as JS
--                   ( askJSM, jsg, js, jss, jsf, js0,JSContextRef, JSM, JSF)
-- import qualified Language.Javascript.JSaddle.Classes as JS
-- import qualified Language.Javascript.JSaddle.Value as JSV (valMakeNumber, strictEqual)
--
-- import           RouteTH
--


inserts :: (Hashable k, Eq k) => [(k,v)] -> HM.HashMap k v -> HM.HashMap k v
inserts = flip $ foldl' (\m (k,v) -> HM.insert k v m)


mkUTCTime :: Text -> UTCTime
mkUTCTime txt = txtToUTC txt
  where year txt' = read $ T.unpack $ T.take 4 txt' :: Integer
        month txt' = read $ T.unpack $ T.take 2 $ T.drop 4 txt' :: Int
        day txt' = read $ T.unpack $ T.take 2 $ T.drop 6 txt' :: Int
        hours txt' = read $ T.unpack $ T.take 2 (T.drop 8 txt') :: Integer
        mins txt' = read $ T.unpack $ T.take 2 $ T.drop 2 $ (T.drop 8 txt') :: Integer
        secs txt' = read $ T.unpack $ T.take 2 $ T.drop 4 $ (T.drop 8 txt') :: Integer
        txtToUTC txt' = UTCTime (fromGregorian (year txt') (month txt') (day txt')) (secondsToDiffTime $ (hours txt')*3600 + (mins txt')*60 + (secs txt'))


umkUTCTime :: UTCTime -> String
umkUTCTime time = utcTotxt time
  where (year,month,day) = toGregorian $ utctDay $ time
        dtime = truncate $ utctDayTime time :: Int
        hours = dtime `div` 3600
        mins = (dtime `mod` 3600) `div` 60
        secs = dtime `mod` 60
        utcTotxt _t' = (show $ year) ++ (attachZero $ show month) ++ (attachZero $ show day) ++ (attachZero $ show hours) ++ (attachZero $ show mins) ++ (attachZero $ show secs)


attachZero :: String -> String
attachZero str = if ((length str) == 1) then ("0" ++ str) else str


getTimeDiffTextFromToday :: NominalDiffTime -> IO Text
getTimeDiffTextFromToday dtime = do
  ctime <- getCurrentTime
  let ptime = addUTCTime (-dtime) ctime
      ctt = umkUTCTime ctime
      ptt = umkUTCTime ptime
  return (T.pack $ ptt ++ ctt)


testAddDays :: Integer -> UTCTime -> Day
testAddDays n time =
  let day = utctDay time
  in addDays n day


hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return
