module Com.Example.Utility (

) where
import qualified Data.Aeson
import qualified Data.Function
import qualified Data.Maybe
import qualified Data.Text.Encoding
import qualified Network.HTTP.Date

instance Data.Aeson.ToJSON Network.HTTP.Date.HTTPDate where
    toJSON = Data.Aeson.String . Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate

instance Data.Aeson.FromJSON Network.HTTP.Date.HTTPDate where
    parseJSON = Data.Aeson.withText "Network.HTTP.Date.HTTPDate" $ \t -> 
        Network.HTTP.Date.parseHTTPDate (Data.Text.Encoding.encodeUtf8 t)
                    Data.Function.& Data.Maybe.maybe (fail "Failed to parse HTTP date") pure
        
    


