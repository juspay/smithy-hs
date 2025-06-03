{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TemperaturePreference (
    TemperaturePreference(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

-- Enum implementation for TemperaturePreference
data TemperaturePreference =
    HOT
    | ICED
    | EXTRA_HOT
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON TemperaturePreference where
    toJSON HOT = Data.Aeson.String $ Data.Text.pack "HOT"
    toJSON ICED = Data.Aeson.String $ Data.Text.pack "ICED"
    toJSON EXTRA_HOT = Data.Aeson.String $ Data.Text.pack "EXTRA_HOT"

instance Data.Aeson.FromJSON TemperaturePreference where
    parseJSON = Data.Aeson.withText "TemperaturePreference" $ \v ->
        case v of
            "HOT" -> pure HOT
            "ICED" -> pure ICED
            "EXTRA_HOT" -> pure EXTRA_HOT
            _ -> fail $ "Unknown value for TemperaturePreference: " <> Data.Text.unpack v
        
    


