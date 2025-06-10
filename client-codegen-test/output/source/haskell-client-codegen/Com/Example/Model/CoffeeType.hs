module Com.Example.Model.CoffeeType (
    CoffeeType(..)
) where
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show

-- Enum implementation for CoffeeType
data CoffeeType =
    DRIP
    | POUR_OVER
    | LATTE
    | ESPRESSO
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON CoffeeType where
    toJSON DRIP = Data.Aeson.String $ Data.Text.pack "Drip"
    toJSON POUR_OVER = Data.Aeson.String $ Data.Text.pack "POUR_OVER"
    toJSON LATTE = Data.Aeson.String $ Data.Text.pack "LATTE"
    toJSON ESPRESSO = Data.Aeson.String $ Data.Text.pack "ESPRESSO"

instance Com.Example.Utility.RequestSegment CoffeeType where
    toRequestSegment DRIP = "Drip"
    toRequestSegment POUR_OVER = "POUR_OVER"
    toRequestSegment LATTE = "LATTE"
    toRequestSegment ESPRESSO = "ESPRESSO"

instance Data.Aeson.FromJSON CoffeeType where
    parseJSON = Data.Aeson.withText "CoffeeType" $ \v ->
        case v of
            "Drip" -> pure DRIP
            "POUR_OVER" -> pure POUR_OVER
            "LATTE" -> pure LATTE
            "ESPRESSO" -> pure ESPRESSO
            _ -> fail $ "Unknown value for CoffeeType: " <> Data.Text.unpack v
        
    

instance Com.Example.Utility.ResponseSegment CoffeeType where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "Drip" -> Data.Either.Right DRIP
        Data.Either.Right "POUR_OVER" -> Data.Either.Right POUR_OVER
        Data.Either.Right "LATTE" -> Data.Either.Right LATTE
        Data.Either.Right "ESPRESSO" -> Data.Either.Right ESPRESSO
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    


