module Com.Example.Model.CoffeeType (
    CoffeeType(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
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
        
    


