
module Forecast.Cache where

import Control.Monad
import qualified Data.Map.Strict as M
import Forecast.Types
import qualified Text.Read as R
import System.Directory
import System.IO

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
