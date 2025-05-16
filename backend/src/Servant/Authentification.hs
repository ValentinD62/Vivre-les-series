{-# LANGUAGE OverloadedStrings #-}
module Authentification where
    
import Data.Text (Text)
import Servant
import Servant.API
import Servant.Server
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import User
import Database.Selda

newtype Token = Token Text deriving (Show)

verifyToken :: Token -> IO (Maybe User)
verifyToken (Token token) = do
  -- Ici, vous pouvez ajouter la logique pour vérifier le token
  -- et extraire les informations de l'utilisateur.
  -- Pour cet exemple, nous simulons simplement un utilisateur valide.
  return $ if token == "valid-token" then Just (User (toId 1) "Alice" "ez" " e" "eza" ) else Nothing