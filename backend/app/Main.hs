{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Servant
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Directory (doesFileExist)
import Network.Wai.Middleware.Cors (CorsResourcePolicy, simpleCorsResourcePolicy, cors, corsMethods, corsRequestHeaders, simpleHeaders)
import Control.Monad (when)
import Database.Selda.SQLite
import Database.Selda.Backend (runSeldaT)
import Network.Wai.Handler.Warp (run)
import qualified Data.Text.IO as TIO
import System.IO (hSetEncoding, stdout, utf8)
import Prelude
import qualified Data.Text as T
import CreateDB
import ServantMain
import System.Environment (lookupEnv)
import TMDBApi
import MovieExtern
import UseCaseSerie



dbFilename :: String
dbFilename = "serie.db"


-- | Serveur permettant de créer les routes pour l'accès à la bdd
-- /comment/pull/:num/ Permet de récupérer un fichier json des commentaires en fonction de l'id du lieu
-- /comment/create Permet d'ajouter un commentaire dans la base de donnée
-- main :: IO ()
-- main = do
--   -- dbExists <- doesFileExist dbFilename
--   -- conn <- sqliteOpen dbFilename
--   -- when (not dbExists) $ runSeldaT dbInit conn
--   -- run 8080 $ logStdoutDev app
--   let apiKey = "486a09b6"
--   let movieTitle = "%"
--   result <- fetchMovie apiKey movieTitle
--   case result of
--     Left err  -> putStrLn $ "Erreur: " ++ show err
--     Right movie -> print movie


main :: IO ()
main = do
  hSetEncoding stdout utf8
  apiKey <- lookupEnv "API_KEY"
  dbExists <- doesFileExist dbFilename
  conn <- sqliteOpen dbFilename
  when (not dbExists) $ runSeldaT dbInit conn
  run 8080 $ logStdoutDev app
  case apiKey of
      Just key -> do
        let movieTitle = "Arcane"
        mapM_ (TIO.putStrLn . name) =<< getMovie movieTitle
      Nothing -> putStrLn "test"

  
