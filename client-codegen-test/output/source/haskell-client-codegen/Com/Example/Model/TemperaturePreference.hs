module Com.Example.Model.TemperaturePreference (
    TemperaturePreference(..)
) where
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
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
        
    

instance Com.Example.Utility.SerDe TemperaturePreference where
    serializeElement HOT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "HOT"
    serializeElement ICED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "ICED"
    serializeElement EXTRA_HOT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "EXTRA_HOT"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "HOT" -> Right HOT
        "ICED" -> Right ICED
        "EXTRA_HOT" -> Right EXTRA_HOT
        e -> Left ("Failed to de-serialize TemperaturePreference, encountered unknown variant: " ++ (show bs))
    


