module Com.Example.Model.TestHttpLabelsOutput (
    build,
    TestHttpLabelsOutputBuilder,
    TestHttpLabelsOutput
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

data TestHttpLabelsOutput = TestHttpLabelsOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpLabelsOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpLabelsOutput

instance Data.Aeson.FromJSON TestHttpLabelsOutput where
    parseJSON = Data.Aeson.withObject "TestHttpLabelsOutput" $ \_ -> pure $ TestHttpLabelsOutput



data TestHttpLabelsOutputBuilderState = TestHttpLabelsOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpLabelsOutputBuilderState
defaultBuilderState = TestHttpLabelsOutputBuilderState {
}

type TestHttpLabelsOutputBuilder = Control.Monad.State.Strict.State TestHttpLabelsOutputBuilderState


build :: TestHttpLabelsOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpLabelsOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (TestHttpLabelsOutput { 
    })


instance Com.Example.Utility.FromResponseParser TestHttpLabelsOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        
        pure $ TestHttpLabelsOutput {
            
        }

