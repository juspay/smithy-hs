{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.CoffeeCustomization (
    CoffeeCustomization(..)
) where
import qualified Com.Example.Model.MilkType
import qualified Com.Example.Model.TemperaturePreference
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Functor
import qualified GHC.Generics
import qualified GHC.Show

-- Union implementation for CoffeeCustomization
data CoffeeCustomization =
    Milk (Com.Example.Model.MilkType.MilkType)
    | Temperature (Com.Example.Model.TemperaturePreference.TemperaturePreference)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show
    )

instance Data.Aeson.ToJSON CoffeeCustomization where
    toJSON (Milk a) = Data.Aeson.object [ "milk" Data.Aeson..= a ]
    toJSON (Temperature a) = Data.Aeson.object [ "temperature" Data.Aeson..= a ]

instance Data.Aeson.FromJSON CoffeeCustomization where
    parseJSON = Data.Aeson.withObject "CoffeeCustomization" $ \v ->
        (Milk Data.Functor.<$> v Data.Aeson..: "milk") Control.Applicative.<|>
        (Temperature Data.Functor.<$> v Data.Aeson..: "temperature") Control.Applicative.<|>
        fail "Could not parse CoffeeCustomization. Expected an object with one of keys: milk, temperature."
    


