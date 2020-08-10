{-# LANGUAGE FlexibleContexts #-}

module Forecast.Request where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Forecast.Types
import Network.HTTP.Req
import Text.URI

userAgent :: T.Text -> Option s
userAgent emailAddress = header
  (encodeUtf8 "User-Agent")
  (encodeUtf8 ("(My CLI Weather App, " <> emailAddress <> ")" :: T.Text))

queryPositionStack :: (Monad m, MonadHttp m, MonadReader Config m) => T.Text -> m (JsonResponse Value)
queryPositionStack address = do
  config <- ask
  -- positionstack's free plan only allows for http ¯\_(ツ)_/¯
  let (uri, options) = fromJust $ mkURI "http://api.positionstack.com/v1/forward" >>= useHttpURI
      accessKeyQP = "access_key" =: (positionStackAccessKey config)
      queryQP = "query" =: address
      options' = options <> accessKeyQP <> queryQP
  req GET uri NoReqBody jsonResponse options'

getWeatherGov :: (Monad m, MonadHttp m, MonadReader Config m) => T.Text -> m (JsonResponse Value)
getWeatherGov url = do
  config <- ask
  let (uri, options) = fromJust $ mkURI url >>= useHttpsURI
      options' = options <> (userAgent $ emailAddress config)
  req GET uri NoReqBody jsonResponse options'

getCoordinates :: Value -> T.Text
getCoordinates json = T.pack $ L.intercalate "," coordPair
  where coordPair = json ^.. key "data"
                    . _Array
                    . taking 1 traversed
                    . (key "latitude" <> key "longitude")
                    . _Number
                    . to show

getForecastUrl :: Value -> Maybe T.Text
getForecastUrl = preview $ key "properties" . key "forecast" . _String

getLatestForecast :: (Monad m, MonadIO m, MonadHttp m, MonadReader Config m) => Address -> m Value
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
