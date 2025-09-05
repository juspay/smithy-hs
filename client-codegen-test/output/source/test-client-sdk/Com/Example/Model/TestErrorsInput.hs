module Com.Example.Model.TestErrorsInput (
    build,
    TestErrorsInputBuilder,
    TestErrorsInput
) where
import qualified Com.Example.Utility
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Types.Method

data TestErrorsInput = TestErrorsInput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestErrorsInput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Com.Example.Utility.SerializeBody TestErrorsInput

instance Data.Aeson.FromJSON TestErrorsInput where
    parseJSON = Data.Aeson.withObject "TestErrorsInput" $ \_ -> pure $ TestErrorsInput



data TestErrorsInputBuilderState = TestErrorsInputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestErrorsInputBuilderState
defaultBuilderState = TestErrorsInputBuilderState {
}

type TestErrorsInputBuilder = Control.Monad.State.Strict.State TestErrorsInputBuilderState


build :: TestErrorsInputBuilder () -> Data.Either.Either Data.Text.Text TestErrorsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (TestErrorsInput { 
    })


instance Com.Example.Utility.IntoRequestBuilder TestErrorsInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Com.Example.Utility.setPath [
            "error-4xx"
            ]
        
        
        

