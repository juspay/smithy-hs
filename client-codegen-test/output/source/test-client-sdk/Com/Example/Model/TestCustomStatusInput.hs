module Com.Example.Model.TestCustomStatusInput (
    build,
    TestCustomStatusInputBuilder,
    TestCustomStatusInput
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

data TestCustomStatusInput = TestCustomStatusInput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestCustomStatusInput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Com.Example.Utility.SerializeBody TestCustomStatusInput

instance Data.Aeson.FromJSON TestCustomStatusInput where
    parseJSON = Data.Aeson.withObject "TestCustomStatusInput" $ \_ -> pure $ TestCustomStatusInput



data TestCustomStatusInputBuilderState = TestCustomStatusInputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestCustomStatusInputBuilderState
defaultBuilderState = TestCustomStatusInputBuilderState {
}

type TestCustomStatusInputBuilder = Control.Monad.State.Strict.State TestCustomStatusInputBuilderState


build :: TestCustomStatusInputBuilder () -> Data.Either.Either Data.Text.Text TestCustomStatusInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (TestCustomStatusInput { 
    })


instance Com.Example.Utility.IntoRequestBuilder TestCustomStatusInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Com.Example.Utility.setPath [
            "custom-status"
            ]
        
        
        

