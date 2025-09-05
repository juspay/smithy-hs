module Com.Example.Model.TestQueryOutput (
    setMessage,
    build,
    TestQueryOutputBuilder,
    TestQueryOutput,
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

data TestQueryOutput = TestQueryOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestQueryOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    

instance Com.Example.Utility.SerializeBody TestQueryOutput

instance Data.Aeson.FromJSON TestQueryOutput where
    parseJSON = Data.Aeson.withObject "TestQueryOutput" $ \v -> TestQueryOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestQueryOutputBuilderState = TestQueryOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestQueryOutputBuilderState
defaultBuilderState = TestQueryOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

type TestQueryOutputBuilder = Control.Monad.State.Strict.State TestQueryOutputBuilderState

setMessage :: Data.Text.Text -> TestQueryOutputBuilder ()
setMessage value =
   Control.Monad.State.Strict.modify (\s -> (s { messageBuilderState = Data.Maybe.Just value }))

build :: TestQueryOutputBuilder () -> Data.Either.Either Data.Text.Text TestQueryOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestQueryOutput.TestQueryOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestQueryOutput { 
        message = message'
    })


instance Com.Example.Utility.FromResponseParser TestQueryOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Com.Example.Utility.deSerField "message"
        pure $ TestQueryOutput {
            message = var0
        }

