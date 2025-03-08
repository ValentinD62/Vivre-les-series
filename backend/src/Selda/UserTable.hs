{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module UserTable where

import User

import Database.Selda
import Database.Selda.SQLite

-- | Déclaration des contraintes de la table user
userTable :: Table User
userTable = table "user" 
  [ #user_id :- autoPrimary ]


selectAllUser :: IO [User]
selectAllUser = do
    let users = query $ select userTable
    withSQLite "serie.db" users

select2User :: IO[User]
select2User = withSQLite "serie.db" selectUser

selectOutUserConnection :: Text -> Text -> IO [User]
selectOutUserConnection pseudo password = withSQLite "serie.db" $ selectUserConnection pseudo password

selectUserConnection :: Text -> Text -> SeldaT SQLite IO[User]
selectUserConnection pseudo password = query $ do
  user <- select userTable
  restrict (user ! #user_firstname .== literal pseudo)
  restrict (user ! #user_password .== literal password)
  return user

selectUser :: SeldaT SQLite IO[User]
selectUser = 
    query $ do
    user <- select userTable
    restrict (user ! #user_firstname .== "Hugoat")    
    return user


insertOutUser :: User -> IO()
insertOutUser u = liftIO(withSQLite "serie.db" $ insertUser u)

insertUser :: User -> SeldaT SQLite IO()
insertUser (User _ firstname lastname password picture ) = 
  tryInsert userTable
        [User def  firstname lastname password picture]
        >>= liftIO . print