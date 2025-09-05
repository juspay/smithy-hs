module Com.Example.Model.TestErrorsOutput (
    build,
    TestErrorsOutputBuilder,
    TestErrorsOutput
) where
import qualified Com.Example.Utility
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Types

data TestErrorsOutput = TestErrorsOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestErrorsOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Com.Example.Utility.SerializeBody TestErrorsOutput

instance Data.Aeson.FromJSON TestErrorsOutput where
    parseJSON = Data.Aeson.withObject "TestErrorsOutput" $ \_ -> pure $ TestErrorsOutput



data TestErrorsOutputBuilderState = TestErrorsOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestErrorsOutputBuilderState
defaultBuilderState = TestErrorsOutputBuilderState {
}

type TestErrorsOutputBuilder = Control.Monad.State.Strict.State TestErrorsOutputBuilderState


build :: TestErrorsOutputBuilder () -> Data.Either.Either Data.Text.Text TestErrorsOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (TestErrorsOutput { 
    })


instance Com.Example.Utility.FromResponseParser TestErrorsOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        
        pure $ TestErrorsOutput {
            
        }

