module Com.Example.Model.TemperaturePreference (
    TemperaturePreference(..)
) where
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Either
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

instance Com.Example.Utility.RequestSegment TemperaturePreference where
    toRequestSegment HOT = "HOT"
    toRequestSegment ICED = "ICED"
    toRequestSegment EXTRA_HOT = "EXTRA_HOT"

instance Data.Aeson.FromJSON TemperaturePreference where
    parseJSON = Data.Aeson.withText "TemperaturePreference" $ \v ->
        case v of
            "HOT" -> pure HOT
            "ICED" -> pure ICED
            "EXTRA_HOT" -> pure EXTRA_HOT
            _ -> fail $ "Unknown value for TemperaturePreference: " <> Data.Text.unpack v
        
    

instance Com.Example.Utility.ResponseSegment TemperaturePreference where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "HOT" -> Data.Either.Right HOT
        Data.Either.Right "ICED" -> Data.Either.Right ICED
        Data.Either.Right "EXTRA_HOT" -> Data.Either.Right EXTRA_HOT
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    


