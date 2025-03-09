{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Directory (doesFileExist)
import Network.Wai.Middleware.Cors (CorsResourcePolicy, simpleCorsResourcePolicy, cors, corsMethods, corsRequestHeaders, simpleHeaders)
import Control.Monad (when)
import Database.Selda.SQLite
import Database.Selda.Backend (runSeldaT)
import Network.Wai.Handler.Warp (run)


import CreateDB
import ServantMain


dbFilename :: String
dbFilename = "serie.db"


-- | Serveur permettant de créer les routes pour l'accès à la bdd
-- /comment/pull/:num/ Permet de récupérer un fichier json des commentaires en fonction de l'id du lieu
-- /comment/create Permet d'ajouter un commentaire dans la base de donnée
main :: IO ()
main = do
  dbExists <- doesFileExist dbFilename
  conn <- sqliteOpen dbFilename
  when (not dbExists) $ runSeldaT dbInit conn
  run 8080 $ logStdoutDev app



policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
    { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = simpleHeaders
    }