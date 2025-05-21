{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.InternalServerError (
    InternalServerError
) where
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics

data InternalServerError = InternalServerError {
    message :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

