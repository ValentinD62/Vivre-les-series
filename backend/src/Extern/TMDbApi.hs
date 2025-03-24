{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TMDbApi where

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
import Network.HTTP.Client (Request, parseRequest, requestHeaders)



type TMDbAPI = "search" :> "tv"
  :> QueryParam "query" Text
  :> Header "Authorization" Text
  :> Get '[JSON] TMDbResponse


tmdbClient :: Maybe Text -> Maybe Text -> ClientM TMDbResponse
tmdbClient = client (Proxy :: Proxy TMDbAPI)

fetchMovie :: Text -> Text -> IO (Either ClientError TMDbResponse)
fetchMovie title bearerToken = do
    manager <- newManager tlsManagerSettings
    let env = mkClientEnv manager (BaseUrl Https "api.themoviedb.org" 443 "/3")
    runClientM (tmdbClient (Just title) (Just ("Bearer " <> bearerToken))) env