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

module UserServant where

import Prelude.Compat
import Servant
import Database.Selda
import Database.Selda.SQLite
import Prelude ()


import UserTable
import User
import PullUser


type SelectUsers = "users" :> Get '[JSON][User]
type SelectHugo = "user" :> Get '[JSON][User]
type InsertUser = "user" :> ReqBody '[JSON] User :> Post '[JSON] User
type SelectConnection = "user" :> Capture "pseudo" Text :> Capture "password" Text :> Get '[JSON][User]



handleSelectUsers :: Handler [User]
handleSelectUsers = 
    liftIO pullUser

handleSelectHugo :: Handler [User]
handleSelectHugo = 
    liftIO select2User

handleSelectConnection :: Text -> Text -> Handler [User]
handleSelectConnection pseudo password = liftIO $ selectOutUserConnection pseudo password

