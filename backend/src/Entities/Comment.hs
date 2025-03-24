{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Comment where

import Database.Selda
import Data.Aeson
import Movie
import User

-- | Définition de la table favorite
data Comment = Comment
  { comment_id :: ID Comment
  , user_id :: ID User
  , movie_id :: ID Movie
  , comment_title :: Text
  , comment_body :: Text
  , comment_note :: Int
  } deriving (Generic, Show)

instance SqlRow Comment

-- | Overload pour transformer les champs de la table user en fichier json
instance ToJSON Comment where
  toJSON (Comment cid user_cid movie_cid title body note) =
    object [ "comment_id" .= show cid
           , "user_id" .= show user_cid
           , "movie_id" .= show movie_cid
           , "title" .= title
           , "body" .= body
           , "note" .= note
           ]

-- | Overload pour transformer le fichier JSON en type Comment
instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \u -> do
    cid <- u .: "comment_id"
    cid_user <-  u .: "user_id"
    cid_movie <- u .: "movie_id"
    title <- u .: "title"
    body <- u .: "body"
    note <- u .: "note"
    return $ Comment (toId cid) (toId cid_user) (toId cid_movie) title body note

