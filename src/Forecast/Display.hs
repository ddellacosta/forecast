
module Forecast.Display where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import Text.Tabular
import Text.Tabular.AsciiArt as AsciiArt

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

renderForecast :: Value -> String
renderForecast = AsciiArt.render id id id . tablize
