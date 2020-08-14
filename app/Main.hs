{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader
import Data.Aeson.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Time.Format.ISO8601 as UTC 
import qualified Data.Time.LocalTime as LT
import Dhall
import Forecast.Cache
import Forecast.Display as D
import Forecast.Request
import Forecast.Types
import Network.HTTP.Req
import System.Environment

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
  firstEntryEndTime <- getFirstEntryEndTimeStr cachedJson
                       >>= UTC.iso8601ParseM . T.unpack :: Maybe LT.ZonedTime
  let isNowAfterEndTime = LT.zonedTimeToLocalTime firstEntryEndTime > LT.zonedTimeToLocalTime now
  cachedJson <$ guard isNowAfterEndTime

getForecast :: (MonadIO m, MonadHttp m, MonadReader Config m) => Address -> Cache -> m ForecastJson
getForecast address cache = do
  now <- liftIO $ LT.getZonedTime
  case getCachedForecastJson now address cache of
    Just cachedJson -> liftIO $ putStrLn "Using cached JSON"
                       >> pure cachedJson
    Nothing         -> getLatestForecast address

doReq :: (Monad m, MonadIO m, MonadHttp m, MonadReader Config m) => T.Text -> m ()
doReq address = do
  cache <- liftIO $ readCache ".cache"
  forecastJson <- getForecast address cache
  liftIO $ putStrLn $ D.renderForecast forecastJson
  let updatedCache = M.insert address forecastJson cache
  liftIO $ writeCache ".cache" updatedCache

main' :: T.Text -> IO ()
main' address = do
  config <- input auto "./config.dhall" :: IO Config
  runReaderT (doReq address) config

main :: IO ()
main = do
  address <- getArgs
  let address' = T.pack $ head address
  main' address'
