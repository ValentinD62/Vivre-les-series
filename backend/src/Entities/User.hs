{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module User where
import Database.Selda
import Data.Aeson


-- | Définition de la table user
data User = User
  { user_id :: ID User
  , user_firstname :: Text
  , user_lastname :: Text
  , user_password :: Text
  , user_picture :: Text
  } deriving (Generic, Show)

instance SqlRow User

-- | Overload pour transformer le type User en fichier json
instance ToJSON User where
  toJSON (User cid firstname lastname password picture) =
    object [ "user_id" .= show cid
           , "user_firstname" .= firstname
           , "user_lastname" .= lastname
           , "user_password" .= password
           , "user_picture" .= picture
           ]

-- | Overload pour transformer le fichier JSON en type User
instance FromJSON User where
  parseJSON = withObject "User" $ \u -> do
    cid <- u .: "user_id"
    firstname <- u.: "user_firstname"
    lastname <- u .: "user_lastname"
    password <- u .: "user_password"
    picture <- u .: "user_picture"
    return $ User (toId cid) firstname lastname password picture