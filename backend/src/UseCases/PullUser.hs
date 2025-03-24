{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}


module PullUser where

import Prelude.Compat
import Prelude ()
import Data.Text

import UserTable
import User

pullUser :: IO [User]
pullUser = selectAllUser

pullUserConnection :: Text -> Text -> IO [User]
pullUserConnection = selectOutUserConnection 

createUser :: User -> IO()
createUser = insertOutUser
