{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Forecast.Types where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Aeson
import Dhall
import Network.HTTP.Req

type Address = T.Text
type Cache = M.Map Address ForecastJson
type CacheString = String
type ForecastJson = Value

data Config = Config
  { positionStackAccessKey :: T.Text
  , emailAddress :: T.Text
  } deriving (Generic, Show)

instance FromDhall Config

newtype ForecastApp a = ForecastApp (ReaderT Config IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance MonadHttp (ReaderT Config IO) where
  handleHttpException e = liftIO $ throwIO e
