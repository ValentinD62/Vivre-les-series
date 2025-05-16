{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ServantMain where

import Servant
import Prelude ()
import Prelude (const, ($))
import Data.Maybe (Maybe(..))
import Network.Wai.Middleware.Cors (CorsResourcePolicy, simpleCorsResourcePolicy, cors, corsMethods, corsRequestHeaders, simpleHeaders)

import UserServant
import TMDBServant

type API
    = SelectUsers
    :<|> SelectHugo
    :<|> SelectConnection
    :<|> InsertUser
    :<|> SelectSerie
    :<|> SelectSerieList

handleServerApi  :: Server API
handleServerApi 
    =    handleSelectUsers
    :<|> handleSelectHugo
    :<|> handleSelectConnection
    :<|> handlePostUser
    :<|> handleSelectSerie
    :<|> handleSelectSerieList

policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
    { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = simpleHeaders
    }

app :: Application
app = cors (const $ Just policy) $ serve (Proxy :: Proxy API) handleServerApi