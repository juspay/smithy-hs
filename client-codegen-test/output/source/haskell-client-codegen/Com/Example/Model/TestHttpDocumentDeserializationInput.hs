module Com.Example.Model.TestHttpDocumentDeserializationInput (
    setCoffeetype,
    build,
    TestHttpDocumentDeserializationInputBuilder,
    TestHttpDocumentDeserializationInput,
    coffeeType
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
import qualified Network.HTTP.Types.Method

data TestHttpDocumentDeserializationInput = TestHttpDocumentDeserializationInput {
    coffeeType :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpDocumentDeserializationInput where
    toJSON a = Data.Aeson.object [
        "coffeeType" Data.Aeson..= coffeeType a
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpDocumentDeserializationInput

instance Data.Aeson.FromJSON TestHttpDocumentDeserializationInput where
    parseJSON = Data.Aeson.withObject "TestHttpDocumentDeserializationInput" $ \v -> TestHttpDocumentDeserializationInput
        Data.Functor.<$> (v Data.Aeson..: "coffeeType")
    



data TestHttpDocumentDeserializationInputBuilderState = TestHttpDocumentDeserializationInputBuilderState {
    coffeeTypeBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpDocumentDeserializationInputBuilderState
defaultBuilderState = TestHttpDocumentDeserializationInputBuilderState {
    coffeeTypeBuilderState = Data.Maybe.Nothing
}

type TestHttpDocumentDeserializationInputBuilder = Control.Monad.State.Strict.State TestHttpDocumentDeserializationInputBuilderState

setCoffeetype :: Data.Maybe.Maybe Data.Text.Text -> TestHttpDocumentDeserializationInputBuilder ()
setCoffeetype value =
   Control.Monad.State.Strict.modify (\s -> (s { coffeeTypeBuilderState = value }))

build :: TestHttpDocumentDeserializationInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpDocumentDeserializationInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    coffeeType' <- Data.Either.Right (coffeeTypeBuilderState st)
    Data.Either.Right (TestHttpDocumentDeserializationInput { 
        coffeeType = coffeeType'
    })


instance Com.Example.Utility.IntoRequestBuilder TestHttpDocumentDeserializationInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Com.Example.Utility.setPath [
            "document_response"
            ]
        Com.Example.Utility.serQuery "type" (coffeeType self)
        
        

