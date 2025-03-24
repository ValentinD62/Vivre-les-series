{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MovieExtern where

import Data.Aeson
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

instance ToJSON MovieExtern where
  toJSON (MovieExtern name adult id first_air_date popularity vote_average overview poster_path) =
    object [ "serie_name" .= show name
           , "serie_adult" .= adult
           , "serie_id" .= id
           , "serie_date" .= first_air_date
           , "serie_popularity" .= popularity
           , "serie_vote" .= vote_average
           , "serie_overview" .= overview
           , "serie_poster" .= poster_path
           ]

data TMDbResponse = TMDbResponse
  { 
    results :: [MovieExtern]
  } deriving (Generic, Show)

instance FromJSON TMDbResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }