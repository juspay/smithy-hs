module Com.Example.Model.MilkType (
    MilkType(..)
) where
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show

-- Enum implementation for MilkType
data MilkType =
    WHOLE
    | SKIM
    | OAT
    | ALMOND
    | SOY
    | NONE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON MilkType where
    toJSON WHOLE = Data.Aeson.String $ Data.Text.pack "WHOLE"
    toJSON SKIM = Data.Aeson.String $ Data.Text.pack "SKIM"
    toJSON OAT = Data.Aeson.String $ Data.Text.pack "OAT"
    toJSON ALMOND = Data.Aeson.String $ Data.Text.pack "ALMOND"
    toJSON SOY = Data.Aeson.String $ Data.Text.pack "SOY"
    toJSON NONE = Data.Aeson.String $ Data.Text.pack "NONE"

instance Data.Aeson.FromJSON MilkType where
    parseJSON = Data.Aeson.withText "MilkType" $ \v ->
        case v of
            "WHOLE" -> pure WHOLE
            "SKIM" -> pure SKIM
            "OAT" -> pure OAT
            "ALMOND" -> pure ALMOND
            "SOY" -> pure SOY
            "NONE" -> pure NONE
            _ -> fail $ "Unknown value for MilkType: " <> Data.Text.unpack v
        
    

instance Com.Example.Utility.SerDe MilkType where
    serializeElement WHOLE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "WHOLE"
    serializeElement SKIM = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "SKIM"
    serializeElement OAT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "OAT"
    serializeElement ALMOND = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "ALMOND"
    serializeElement SOY = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "SOY"
    serializeElement NONE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "NONE"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "WHOLE" -> Right WHOLE
        "SKIM" -> Right SKIM
        "OAT" -> Right OAT
        "ALMOND" -> Right ALMOND
        "SOY" -> Right SOY
        "NONE" -> Right NONE
        e -> Left ("Failed to de-serialize MilkType, encountered unknown variant: " ++ (show bs))
    


