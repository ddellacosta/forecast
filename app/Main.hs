{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Applicative as A
import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Time.LocalTime as LT
import qualified Data.Time.Format.ISO8601 as UTC 
import Debug.Trace
import Network.HTTP.Req
import System.Directory
import System.Environment
import System.IO
import Text.Pretty.Simple
import qualified Text.Read as R
import Text.Tabular
import Text.Tabular.AsciiArt as AsciiArt
import Text.URI

type Address = T.Text
type CacheString = String
type ForecastJson = Value
type Cache = M.Map Address ForecastJson

userAgent :: Option s
userAgent = header
            (encodeUtf8 "User-Agent")
            (encodeUtf8 "(My CLI Weather App, your-email-address-goes-here@domain.com)")

apiAccessKey :: T.Text
apiAccessKey = "<positionstack API access key>"

queryPositionStack :: (Monad m, MonadHttp m) => T.Text -> m (JsonResponse Value)
queryPositionStack address = do
  -- positionstack's free plan only allows for http ¯\_(ツ)_/¯
  let uri = fromJust $ (mkURI "http://api.positionstack.com/v1/forward" :: Maybe URI)
  let (uri', options) = fromJust $ (useHttpURI uri)
      accessKeyQP = "access_key" =: apiAccessKey
      queryQP = "query" =: address
      options' = options <> accessKeyQP <> queryQP
  req GET uri' NoReqBody jsonResponse options'

getWeatherGov :: (Monad m, MonadHttp m) => T.Text -> m (JsonResponse Value)
getWeatherGov url = do
  let uri = fromJust $ (mkURI url :: Maybe URI)
  let (uri', options) = fromJust $ (useHttpsURI uri)
      options' = options <> userAgent
  req GET uri' NoReqBody jsonResponse options'

getCoordinates :: Value -> T.Text
getCoordinates json = T.pack $ L.intercalate "," coordPair
  where coordPair = json ^.. key "data"
                    . _Array
                    . taking 1 traversed
                    . (key "latitude" <> key "longitude")
                    . _Number
                    . to show

getForecastUrl :: Value -> Maybe T.Text
getForecastUrl = preview (key "properties" . key "forecast" . _String)

getLatestForecast :: (Monad m, MonadIO m, MonadHttp m)
                  => Address
                  -> m Value
getLatestForecast address = do
  -- first we get lat/long coordinates we can use with weather.gov via
  -- positionstack.com
  coordsResp <- queryPositionStack address
  let coords = getCoordinates $ responseBody coordsResp
  liftIO $ putStrLn $ ("retrieved coordinate pair " ++ show coords ++ " for address " ++ T.unpack address)

  -- then we get the forecastUrl via the weather.gov/points endpoint
  pointsResp <- getWeatherGov (T.append "https://api.weather.gov/points/" coords)
  let forecastUrl = getForecastUrl $ responseBody pointsResp
  liftIO $ putStrLn $ ("retrieved forecastUrl: " ++ show forecastUrl)

  -- finally we can get the forecast for our location
  forecastResp <- getWeatherGov $ fromJust forecastUrl
  pure (responseBody forecastResp)

tablize :: Value -> Table String String String
tablize forecastJson = table
  where periods = forecastJson ^.. key "properties" . key "periods" . _Array . taking 4 traversed
        numericRow keyName = periods ^.. traversed . key keyName . _Number . to show
        stringRow keyName = periods ^.. traversed . key keyName . _String . to T.unpack
        rows = [ numericRow "temperature"
               , zipWith (\s d -> s <> " " <> d) (stringRow "windSpeed") (stringRow "windDirection")
               , stringRow "startTime"
               , stringRow "endTime"
               , stringRow "shortForecast"
               ]
        table = Table
                (Group SingleLine
                 [Group SingleLine
                  [ Header "Temp."
                  , Header "Wind"
                  , Header "Start"
                  , Header "End"
                  , Header "Forecast"
                  ]])
                (Group SingleLine
                 [Group SingleLine $ fmap Header $ stringRow "name"])
                rows

getFirstEntryEndTimeStr :: ForecastJson -> Maybe T.Text
getFirstEntryEndTimeStr = preview $
                            key "properties"
                          . key "periods"
                          . _Array
                          . _head
                          . key "endTime"
                          . _String

getCachedForecastJson :: LT.ZonedTime -> Address -> Cache -> Maybe ForecastJson
getCachedForecastJson now address cache = do
  cachedJson <- M.lookup address cache
  firstEntryEndTimeStr <- getFirstEntryEndTimeStr cachedJson
  firstEntryEndTime <- UTC.iso8601ParseM $ T.unpack firstEntryEndTimeStr :: Maybe LT.ZonedTime
  let isNowAfterEndTime = LT.zonedTimeToLocalTime firstEntryEndTime > LT.zonedTimeToLocalTime now
  cachedJson <$ guard isNowAfterEndTime

getForecast :: (MonadIO m, MonadHttp m) => Address -> Cache -> m ForecastJson
getForecast address cache = do
  now <- liftIO $ LT.getZonedTime
  forecastJson <- case getCachedForecastJson now address cache of
                    Just cachedJson -> liftIO $ putStrLn "Using cached JSON"
                                       >> pure cachedJson
                    Nothing         -> getLatestForecast address
  pure forecastJson

readCache' :: CacheString -> Cache
readCache' cacheString = maybe M.empty id (R.readMaybe cacheString :: Maybe Cache)

readCache :: FilePath -> IO Cache
readCache filePath = do
  handle <- openFile filePath ReadWriteMode
  cacheString <- hGetContents handle
  let cache = readCache' cacheString
  when (length cacheString > 0) $ hClose handle
  pure cache

writeCache :: FilePath -> Cache -> IO ()
writeCache filePath cache = do
  writeFile (filePath ++ ".new") $ show cache
  renameFile (filePath ++ ".new") filePath

doReq :: (Monad m, MonadIO m, MonadHttp m) => T.Text -> m ()
doReq address = do
  cache <- liftIO $ readCache ".cache"
  forecastJson <- getForecast address cache
  liftIO $ putStrLn $ AsciiArt.render id id id $ tablize forecastJson
  let updatedCache = M.insert address forecastJson cache
  liftIO $ writeCache ".cache" updatedCache

main' :: T.Text -> IO ()
main' address = runReq defaultHttpConfig (doReq address)

main :: IO ()
main = do
  address <- getArgs
  let address' = T.pack $ head address
  main' address'
