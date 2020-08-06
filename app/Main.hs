{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
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
import System.Environment
import System.IO
import Text.Pretty.Simple
import qualified Text.Read as R
import Text.Tabular
import Text.Tabular.AsciiArt as AsciiArt
import Text.URI

type Address = T.Text
type ForecastJson = Value
type ForecastCache = M.Map Address ForecastJson

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
                  -> ForecastCache
                  -> m Value
getLatestForecast address cache = do
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

-- shamelessly stolen from https://stackoverflow.com/a/31342939
-- (and tweaked slightly)
getLines :: Handle -> IO String
getLines handle = do
  isEof <- hIsEOF handle
  if (not isEof)
    then do
      line <- (hGetLine handle)
      pure line <> getLines handle
    else
      pure []

getFirstEntryEndTime :: ForecastJson -> Maybe LT.ZonedTime
getFirstEntryEndTime forecastJson = UTC.iso8601ParseM
  $ T.unpack
  $ fromJust -- dynamic typing simulator
  $ forecastJson ^? key "properties" . key "periods" . _Array . _head . key "endTime" . _String

getCachedForecastJson :: LT.ZonedTime -> Address -> ForecastCache -> Maybe ForecastJson
getCachedForecastJson now address cache = do
  cachedJson <- M.lookup address cache
  firstEntryEndTime <- getFirstEntryEndTime cachedJson
  if (LT.zonedTimeToLocalTime firstEntryEndTime > LT.zonedTimeToLocalTime now)
    then pure cachedJson
    else Nothing

doReq :: (Monad m, MonadIO m, MonadHttp m) => Handle -> T.Text -> m ()
doReq handle address = do
  cacheString <- liftIO $ getLines handle
  -- reset position so we can write from beginning
  liftIO $ hSeek handle AbsoluteSeek 0
  let cache = maybe M.empty id
              (R.readMaybe cacheString :: Maybe (M.Map Address ForecastJson))
  now <- liftIO $ LT.getZonedTime
  forecastJson <- case (getCachedForecastJson now address cache) of
                    Just cachedJson -> liftIO $ putStrLn "Using cached JSON response"
                                        >> pure cachedJson
                    Nothing         -> (getLatestForecast address cache)
  liftIO $ putStrLn $ AsciiArt.render id id id $ tablize forecastJson
  liftIO $ hPutStrLn handle (show $ M.insert address forecastJson cache)
  liftIO $ hFlush handle
 
main' :: T.Text -> IO ()
main' address =
  withFile ".cache" ReadWriteMode $
  \handle -> runReq defaultHttpConfig (doReq handle address)

main :: IO ()
main = do
  address <- getArgs
  let address' = T.pack $ head address
  main' address'
