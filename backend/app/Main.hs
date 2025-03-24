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
import TMDbApi
import MovieExtern


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

-- main :: IO ()
-- main = do
--   hSetEncoding stdout utf8
--   -- let bearerToken = "eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiJkNDg4YjI5MTkwMmNlMTkxNjEzODFkODM0Mjc2YTM4NiIsIm5iZiI6MTc0MjExODkzNC41NDQ5OTk4LCJzdWIiOiI2N2Q2YTAxNjE5MTg2OGM1NGZmMTg1ZTUiLCJzY29wZXMiOlsiYXBpX3JlYWQiXSwidmVyc2lvbiI6MX0.Kswy-QiOG6wTSXC-k6d04pp68zRR0lapT1bpcHCBLKc"
--   apiKey <- lookupEnv "API_KEY"

--   case apiKey of
--       Just key -> do
--         let apiKeyT = T.pack key
--         let movieTitle = "Arcane"
--         fetchMovie movieTitle apiKeyT >>= \case
--           Left err -> putStrLn $ "Erreur: " ++ show err
--           Right (TMDbResponse movies) -> do
--             putStrLn "Films trouvés :"
--             mapM_ (TIO.putStrLn . name) movies
--       Nothing  -> putStrLn "API Key not found!" 
  
  



policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
    { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = simpleHeaders
    }