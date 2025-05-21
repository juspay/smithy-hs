{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.CoffeeType (
    CoffeeType
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified GHC.Generics

-- Enum implementation for CoffeeType
data CoffeeType =
    DRIP
    | POUR_OVER
    | LATTE
    | ESPRESSO
    deriving (GHC.Generics.Generic, Data.Eq.Eq)

instance Data.Aeson.ToJSON CoffeeType where
    toJSON DRIP = Data.Aeson.String $ Data.Text.pack "Drip"
    toJSON POUR_OVER = Data.Aeson.String $ Data.Text.pack "POUR_OVER"
    toJSON LATTE = Data.Aeson.String $ Data.Text.pack "LATTE"
    toJSON ESPRESSO = Data.Aeson.String $ Data.Text.pack "ESPRESSO"


