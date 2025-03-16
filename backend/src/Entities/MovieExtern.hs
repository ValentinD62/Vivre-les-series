{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MovieExtern where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Casing (aesonDrop, pascalCase)
import Data.Text (Text)
import GHC.Generics (Generic)

data MovieExtern = MovieExtern
  { title :: Text
  , year :: Text
  , genre :: Text
  , plot :: Text
  } deriving (Generic, Show)

instance FromJSON MovieExtern where 
  parseJSON = genericParseJSON  (aesonDrop 0 pascalCase)