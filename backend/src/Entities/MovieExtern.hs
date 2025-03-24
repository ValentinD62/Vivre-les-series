{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MovieExtern where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON, defaultOptions, fieldLabelModifier, camelTo2)
import Data.Aeson.Casing (aesonDrop, pascalCase)
import Data.Text (Text)
import GHC.Generics (Generic)


data MovieExtern = MovieExtern
  { name :: Text
  , adult :: Bool
  , id :: Int
  , first_air_date :: Text
  , popularity :: Float
  , vote_average :: Float
  , overview :: Text
  , poster_path :: Maybe String
  } deriving (Generic, Show)
  
instance FromJSON MovieExtern where 
    parseJSON = genericParseJSON  defaultOptions { fieldLabelModifier = camelTo2 '_' }

data TMDbResponse = TMDbResponse
  { 
    results :: [MovieExtern]
  } deriving (Generic, Show)

instance FromJSON TMDbResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }