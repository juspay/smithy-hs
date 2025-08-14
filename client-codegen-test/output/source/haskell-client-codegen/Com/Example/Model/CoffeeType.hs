module Com.Example.Model.CoffeeType (
    CoffeeType(..)
) where
import qualified Com.Example.Utility
import qualified Data.Aeson
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

instance Data.Aeson.FromJSON CoffeeType where
    parseJSON = Data.Aeson.withText "CoffeeType" $ \v ->
        case v of
            "Drip" -> pure DRIP
            "POUR_OVER" -> pure POUR_OVER
            "LATTE" -> pure LATTE
            "ESPRESSO" -> pure ESPRESSO
            _ -> fail $ "Unknown value for CoffeeType: " <> Data.Text.unpack v
        
    

instance Com.Example.Utility.SerDe CoffeeType where
    serializeElement DRIP = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "Drip"
    serializeElement POUR_OVER = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "POUR_OVER"
    serializeElement LATTE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "LATTE"
    serializeElement ESPRESSO = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "ESPRESSO"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "Drip" -> Right DRIP
        "POUR_OVER" -> Right POUR_OVER
        "LATTE" -> Right LATTE
        "ESPRESSO" -> Right ESPRESSO
        e -> Left ("Failed to de-serialize CoffeeType, encountered unknown variant: " ++ (show bs))
    


