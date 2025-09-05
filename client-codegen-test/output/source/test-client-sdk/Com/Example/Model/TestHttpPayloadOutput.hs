module Com.Example.Model.TestHttpPayloadOutput (
    setMessage,
    build,
    TestHttpPayloadOutputBuilder,
    TestHttpPayloadOutput,
    message
) where
import qualified Com.Example.Utility
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Types

data TestHttpPayloadOutput = TestHttpPayloadOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpPayloadOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpPayloadOutput

instance Data.Aeson.FromJSON TestHttpPayloadOutput where
    parseJSON = Data.Aeson.withObject "TestHttpPayloadOutput" $ \v -> TestHttpPayloadOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestHttpPayloadOutputBuilderState = TestHttpPayloadOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpPayloadOutputBuilderState
defaultBuilderState = TestHttpPayloadOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

type TestHttpPayloadOutputBuilder = Control.Monad.State.Strict.State TestHttpPayloadOutputBuilderState

setMessage :: Data.Text.Text -> TestHttpPayloadOutputBuilder ()
setMessage value =
   Control.Monad.State.Strict.modify (\s -> (s { messageBuilderState = Data.Maybe.Just value }))

build :: TestHttpPayloadOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpPayloadOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpPayloadOutput.TestHttpPayloadOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestHttpPayloadOutput { 
        message = message'
    })


instance Com.Example.Utility.FromResponseParser TestHttpPayloadOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Com.Example.Utility.deSerField "message"
        pure $ TestHttpPayloadOutput {
            message = var0
        }

