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


module TMDBServant where

import Prelude.Compat
import Servant
import Database.Selda
import Database.Selda.SQLite ()
import UseCaseSerie
import Prelude ()

import MovieExtern

type SelectSerie = "serie" :> Capture "titre" Text :> Get '[JSON][MovieExtern]

handleSelectSerie :: Text -> Handler [MovieExtern]
handleSelectSerie titre = liftIO $ getMovie titre


