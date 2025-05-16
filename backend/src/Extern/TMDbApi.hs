{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TMDBApi where

import MovieExtern
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Servant.Client(ClientM)

import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Prelude 
import Data.Proxy

import qualified Data.Text as T
import Network.HTTP.Client (Request, parseRequest, requestHeaders)
import System.IO (hSetEncoding, stdout, utf8)
import System.Environment (lookupEnv)
import Data.Text(pack) 
import qualified Data.Text.IO as TIO


type TMDbAPISerie = "search" :> "tv"
  :> QueryParam "query" Text
  :> Header "Authorization" Text
  :> Get '[JSON] TMDbResponse

type TMDbAPISerieListe = Header "Authorization" Text :> Get '[JSON] TMDbResponse


tmdbClientSerie :: Maybe Text -> Maybe Text -> ClientM TMDbResponse
tmdbClientSerie = client (Proxy :: Proxy TMDbAPISerie)

tmdbClientSerieList :: Maybe Text -> ClientM TMDbResponse
tmdbClientSerieList = client (Proxy :: Proxy TMDbAPISerieListe)

fetchMovie :: Text -> Text -> IO (Either ClientError TMDbResponse)
fetchMovie title bearerToken = do
    manager <- newManager tlsManagerSettings
    let env = mkClientEnv manager (BaseUrl Https "api.themoviedb.org" 443 "/3")
    runClientM (tmdbClientSerie (Just title) (Just ("Bearer " <> bearerToken))) env

fetchSerieList :: Text -> IO (Either ClientError TMDbResponse)
fetchSerieList  bearerToken = do
    manager <- newManager tlsManagerSettings
    let env = mkClientEnv manager (BaseUrl Https "api.themoviedb.org" 443 "/3/trending/tv/day?language=fr")
    runClientM (tmdbClientSerieList  (Just ("Bearer " <> bearerToken))) env


getOutMovie :: Text -> IO [MovieExtern]
getOutMovie title = do
   hSetEncoding stdout utf8
   apiKey <- lookupEnv "API_KEY"

   case apiKey of
       Just key -> do
         let apiKeyT = T.pack key
         fetchMovie title apiKeyT >>= \case
           Left _ -> return []
           Right (TMDbResponse movies) -> do
             return movies
       Nothing  -> return []
  
getOutSerieList :: IO [MovieExtern]
getOutSerieList = do
  hSetEncoding stdout utf8
  apiKey <- lookupEnv "API_KEY"
  case apiKey of
       Just key -> do
         let apiKeyT = T.pack key
         fetchSerieList apiKeyT >>= \case
           Left _ -> return []
           Right (TMDbResponse movies) -> do
             return movies
       Nothing  -> return []