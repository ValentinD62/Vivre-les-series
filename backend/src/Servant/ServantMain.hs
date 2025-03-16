{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ServantMain where

import Servant
import Prelude ()

import UserServant

type API
    = SelectUsers
    :<|> SelectHugo
    :<|> SelectConnection
    :<|> InsertUser

handleServerApi  :: Server API
handleServerApi 
    =    handleSelectUsers
    :<|> handleSelectHugo
    :<|> handleSelectConnection
    :<|> handlePostUser

app :: Application
app = serve (Proxy :: Proxy API) handleServerApi