module Com.Example.Model.TestCustomStatusOutput (
    setMessage,
    build,
    TestCustomStatusOutputBuilder,
    TestCustomStatusOutput,
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

data TestCustomStatusOutput = TestCustomStatusOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestCustomStatusOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    

instance Com.Example.Utility.SerializeBody TestCustomStatusOutput

instance Data.Aeson.FromJSON TestCustomStatusOutput where
    parseJSON = Data.Aeson.withObject "TestCustomStatusOutput" $ \v -> TestCustomStatusOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestCustomStatusOutputBuilderState = TestCustomStatusOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestCustomStatusOutputBuilderState
defaultBuilderState = TestCustomStatusOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

type TestCustomStatusOutputBuilder = Control.Monad.State.Strict.State TestCustomStatusOutputBuilderState

setMessage :: Data.Text.Text -> TestCustomStatusOutputBuilder ()
setMessage value =
   Control.Monad.State.Strict.modify (\s -> (s { messageBuilderState = Data.Maybe.Just value }))

build :: TestCustomStatusOutputBuilder () -> Data.Either.Either Data.Text.Text TestCustomStatusOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestCustomStatusOutput.TestCustomStatusOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestCustomStatusOutput { 
        message = message'
    })


instance Com.Example.Utility.FromResponseParser TestCustomStatusOutput where
    expectedStatus = Network.HTTP.Types.status201
    responseParser = do
        
        var0 <- Com.Example.Utility.deSerField "message"
        pure $ TestCustomStatusOutput {
            message = var0
        }

