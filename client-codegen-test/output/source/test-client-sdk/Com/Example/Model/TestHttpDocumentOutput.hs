module Com.Example.Model.TestHttpDocumentOutput (
    setMessage,
    build,
    TestHttpDocumentOutputBuilder,
    TestHttpDocumentOutput,
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

data TestHttpDocumentOutput = TestHttpDocumentOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpDocumentOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpDocumentOutput

instance Data.Aeson.FromJSON TestHttpDocumentOutput where
    parseJSON = Data.Aeson.withObject "TestHttpDocumentOutput" $ \v -> TestHttpDocumentOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestHttpDocumentOutputBuilderState = TestHttpDocumentOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpDocumentOutputBuilderState
defaultBuilderState = TestHttpDocumentOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

type TestHttpDocumentOutputBuilder = Control.Monad.State.Strict.State TestHttpDocumentOutputBuilderState

setMessage :: Data.Text.Text -> TestHttpDocumentOutputBuilder ()
setMessage value =
   Control.Monad.State.Strict.modify (\s -> (s { messageBuilderState = Data.Maybe.Just value }))

build :: TestHttpDocumentOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpDocumentOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpDocumentOutput.TestHttpDocumentOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestHttpDocumentOutput { 
        message = message'
    })


instance Com.Example.Utility.FromResponseParser TestHttpDocumentOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Com.Example.Utility.deSerField "message"
        pure $ TestHttpDocumentOutput {
            message = var0
        }

