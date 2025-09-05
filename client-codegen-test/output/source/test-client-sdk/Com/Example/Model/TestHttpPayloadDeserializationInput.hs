module Com.Example.Model.TestHttpPayloadDeserializationInput (
    setCoffeetype,
    build,
    TestHttpPayloadDeserializationInputBuilder,
    TestHttpPayloadDeserializationInput,
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

data TestHttpPayloadDeserializationInput = TestHttpPayloadDeserializationInput {
    coffeeType :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpPayloadDeserializationInput where
    toJSON a = Data.Aeson.object [
        "coffeeType" Data.Aeson..= coffeeType a
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpPayloadDeserializationInput

instance Data.Aeson.FromJSON TestHttpPayloadDeserializationInput where
    parseJSON = Data.Aeson.withObject "TestHttpPayloadDeserializationInput" $ \v -> TestHttpPayloadDeserializationInput
        Data.Functor.<$> (v Data.Aeson..: "coffeeType")
    



data TestHttpPayloadDeserializationInputBuilderState = TestHttpPayloadDeserializationInputBuilderState {
    coffeeTypeBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpPayloadDeserializationInputBuilderState
defaultBuilderState = TestHttpPayloadDeserializationInputBuilderState {
    coffeeTypeBuilderState = Data.Maybe.Nothing
}

type TestHttpPayloadDeserializationInputBuilder = Control.Monad.State.Strict.State TestHttpPayloadDeserializationInputBuilderState

setCoffeetype :: Data.Maybe.Maybe Data.Text.Text -> TestHttpPayloadDeserializationInputBuilder ()
setCoffeetype value =
   Control.Monad.State.Strict.modify (\s -> (s { coffeeTypeBuilderState = value }))

build :: TestHttpPayloadDeserializationInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpPayloadDeserializationInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    coffeeType' <- Data.Either.Right (coffeeTypeBuilderState st)
    Data.Either.Right (TestHttpPayloadDeserializationInput { 
        coffeeType = coffeeType'
    })


instance Com.Example.Utility.IntoRequestBuilder TestHttpPayloadDeserializationInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Com.Example.Utility.setPath [
            "payload_response"
            ]
        Com.Example.Utility.serQuery "type" (coffeeType self)
        
        

