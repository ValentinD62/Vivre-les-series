{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
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

handleServerApi  :: Server API
handleServerApi 
    =    handleSelectUsers
    :<|> handleSelectHugo
    :<|> handleSelectConnection

app :: Application
app = serve (Proxy :: Proxy API) handleServerApi