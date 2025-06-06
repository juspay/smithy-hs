module Com.Example.Model.TestHttpDocumentDeserializationInput (
    setCoffeetype,
    build,
    TestHttpDocumentDeserializationInputBuilder,
    TestHttpDocumentDeserializationInput,
    coffeeType
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

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

newtype TestHttpDocumentDeserializationInputBuilder a = TestHttpDocumentDeserializationInputBuilder {
    runTestHttpDocumentDeserializationInputBuilder :: TestHttpDocumentDeserializationInputBuilderState -> (TestHttpDocumentDeserializationInputBuilderState, a)
}

instance Data.Functor.Functor TestHttpDocumentDeserializationInputBuilder where
    fmap f (TestHttpDocumentDeserializationInputBuilder g) =
        TestHttpDocumentDeserializationInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpDocumentDeserializationInputBuilder where
    pure a = TestHttpDocumentDeserializationInputBuilder (\s -> (s, a))
    (TestHttpDocumentDeserializationInputBuilder f) <*> (TestHttpDocumentDeserializationInputBuilder g) = TestHttpDocumentDeserializationInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpDocumentDeserializationInputBuilder where
    (TestHttpDocumentDeserializationInputBuilder f) >>= g = TestHttpDocumentDeserializationInputBuilder (\s ->
        let (s', a) = f s
            (TestHttpDocumentDeserializationInputBuilder h) = g a
        in h s')

setCoffeetype :: Data.Maybe.Maybe Data.Text.Text -> TestHttpDocumentDeserializationInputBuilder ()
setCoffeetype value =
   TestHttpDocumentDeserializationInputBuilder (\s -> (s { coffeeTypeBuilderState = value }, ()))

build :: TestHttpDocumentDeserializationInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpDocumentDeserializationInput
build builder = do
    let (st, _) = runTestHttpDocumentDeserializationInputBuilder builder defaultBuilderState
    coffeeType' <- Data.Either.Right (coffeeTypeBuilderState st)
    Data.Either.Right (TestHttpDocumentDeserializationInput { 
        coffeeType = coffeeType'
    })


