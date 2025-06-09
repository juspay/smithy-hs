import Data.Aeson
import Data.Function
import Data.Maybe
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Date (HTTPDate, formatHTTPDate, parseHTTPDate)
import Data.Text (Text, pack, unpack, toLower)

instance ToJSON HTTPDate where
    toJSON = Data.Aeson.String . decodeUtf8 . formatHTTPDate

instance FromJSON HTTPDate where
    parseJSON = withText "HTTPDate" $ \t ->
        parseHTTPDate (encodeUtf8 t)
                    & maybe (fail "Failed to parse HTTP date") pure

class RequestSegment a where
    toRequestSegment :: Show a => a -> Text
instance RequestSegment Text where
    toRequestSegment = id
instance RequestSegment Integer where
    toRequestSegment = pack . show
instance RequestSegment Bool where
    toRequestSegment = toLower . pack . show
instance RequestSegment HTTPDate where
    toRequestSegment = decodeUtf8 . formatHTTPDate
