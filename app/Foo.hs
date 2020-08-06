{-# LANGUAGE DeriveGeneric #-}

module Foo where

import qualified GHC.Generics as G
import Data.Aeson
import Data.Aeson.Lens
import Data.HashMap.Strict
import Control.Lens
import Control.Monad

data Color = Red | Yellow | Blue
  deriving (Show, G.Generic)

data Foo = Foo {
    name :: String
  , id :: Int
  , favoriteColor :: Color
  } deriving (Show, G.Generic)

instance ToJSON Color where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Color

instance ToJSON Foo where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Foo

getRecFieldnames rec = (toJSON rec) ^.. _Object . to keys . traversed

-- 
-- λ> getRecFieldnames (Foo "Bob" 1 Red)
-- ["name","id","favoriteColor"]
-- λ> 
-- 

blub :: Int -> IO ()
blub n = do
  putStrLn $ "you gave me a " ++ show n
  unless (1 >= n) $ putStrLn $ show n ++ " is greater than 1"
--  forever $ putStrLn "guess we'll be here for a while"
  replicateM_ 10 $ putStrLn "We'll be here for about...well, exactly 10"
