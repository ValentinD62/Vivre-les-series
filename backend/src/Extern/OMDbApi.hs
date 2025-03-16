{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OMDbApi where

import MovieExtern
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Prelude 
import Data.Proxy

type OMDbAPI = QueryParam' '[Required] "apikey" Text
                   :> QueryParam "t" Text
                   :> Get '[JSON] MovieExtern

omdbClient :: Text -> Maybe Text -> ClientM MovieExtern
omdbClient = client (Proxy :: Proxy OMDbAPI)

fetchMovie :: Text -> Text -> IO (Either ClientError MovieExtern)
fetchMovie apiKey title = do
    manager <- newManager tlsManagerSettings
    let env = mkClientEnv manager (BaseUrl Https "www.omdbapi.com" 443 "")
    runClientM (omdbClient apiKey (Just title)) env