module Com.Example.Model.TestHttpHeadersOutput (
    setMessage,
    build,
    TestHttpHeadersOutputBuilder,
    TestHttpHeadersOutput,
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

data TestHttpHeadersOutput = TestHttpHeadersOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpHeadersOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpHeadersOutput

instance Data.Aeson.FromJSON TestHttpHeadersOutput where
    parseJSON = Data.Aeson.withObject "TestHttpHeadersOutput" $ \v -> TestHttpHeadersOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestHttpHeadersOutputBuilderState = TestHttpHeadersOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpHeadersOutputBuilderState
defaultBuilderState = TestHttpHeadersOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

type TestHttpHeadersOutputBuilder = Control.Monad.State.Strict.State TestHttpHeadersOutputBuilderState

setMessage :: Data.Text.Text -> TestHttpHeadersOutputBuilder ()
setMessage value =
   Control.Monad.State.Strict.modify (\s -> (s { messageBuilderState = Data.Maybe.Just value }))

build :: TestHttpHeadersOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpHeadersOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpHeadersOutput.TestHttpHeadersOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestHttpHeadersOutput { 
        message = message'
    })


instance Com.Example.Utility.FromResponseParser TestHttpHeadersOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Com.Example.Utility.deSerField "message"
        pure $ TestHttpHeadersOutput {
            message = var0
        }

