{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Movie where
import Database.Selda
import Data.Aeson

-- | Définition de la table favorite
data Movie = Movie
  { movie_id :: ID Movie
  , external_id :: Int
  } deriving (Generic, Show)

instance SqlRow Movie

-- | Overload pour transformer les champs de la table user en fichier json
instance ToJSON Movie where
  toJSON (Movie cid external_cid) =
    object [ "favorite_id" .= show cid
           , "user_id" .= show external_cid
           ]
