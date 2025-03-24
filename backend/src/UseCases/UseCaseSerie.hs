{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}


module UseCaseSerie where

import Prelude.Compat
import Prelude ()
import Data.Text


import MovieExtern
import TMDBApi

getMovie :: Text -> IO [MovieExtern]
getMovie = getOutMovie 
