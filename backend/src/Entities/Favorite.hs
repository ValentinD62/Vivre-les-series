{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Favorite where
import Database.Selda
import Data.Aeson

import User
import Movie

-- | Définition de la table favorite
data Favorite = Favorite
  { favorite_id :: ID Favorite
  , user_id :: ID User
  , movie_id :: ID Movie
  , favorite_added_at :: Int
  } deriving (Generic, Show)

instance SqlRow Favorite

-- | Overload pour transformer les champs de la table user en fichier json
instance ToJSON Favorite where
  toJSON (Favorite cid user_cid movie_cid added_at) =
    object [ "favorite_id" .= show cid
           , "user_id" .= show user_cid
           , "movie_id" .= show movie_cid
           , "added_at" .= show added_at
           ]

-- | Overload pour transformer le fichier JSON en type Comment
instance FromJSON Favorite where
  parseJSON = withObject "Favorite" $ \u -> do
    cid <- u .: "favorite_id"
    cid_user <-  u .: "user_id"
    cid_movie <- u .: "movie_id"
    added_at <- u .: "added_at"
    return $ Favorite (toId cid) (toId cid_user) (toId cid_movie) added_at