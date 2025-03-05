{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module CreateDB where

import UserTable
import User

import Database.Selda
import Database.Selda.SQLite

-- | dbInit permet d'initialiser la base de donnée et d'ajouter des données de test
dbInit :: SeldaT SQLite IO ()
dbInit = do
    createTable userTable
    tryInsert userTable
        [ User def "Tom" "Darkes" "1234" "caca.jpg"
        , User def "Hugoat" "Goat" "P0k&m@N" "goat.jpg"] 
        >>= liftIO . print