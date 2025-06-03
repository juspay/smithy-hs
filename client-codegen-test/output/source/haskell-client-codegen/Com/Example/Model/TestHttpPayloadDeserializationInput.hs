{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpPayloadDeserializationInput (
    setCoffeetype,
    build,
    TestHttpPayloadDeserializationInputBuilder,
    TestHttpPayloadDeserializationInput,
    coffeeType
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpPayloadDeserializationInput = TestHttpPayloadDeserializationInput {
    coffeeType :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpPayloadDeserializationInput where
    toJSON a = Data.Aeson.object [
        "coffeeType" Data.Aeson..= coffeeType a
        ]
    


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

newtype TestHttpPayloadDeserializationInputBuilder a = TestHttpPayloadDeserializationInputBuilder {
    runTestHttpPayloadDeserializationInputBuilder :: TestHttpPayloadDeserializationInputBuilderState -> (TestHttpPayloadDeserializationInputBuilderState, a)
}

instance Data.Functor.Functor TestHttpPayloadDeserializationInputBuilder where
    fmap f (TestHttpPayloadDeserializationInputBuilder g) =
        TestHttpPayloadDeserializationInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpPayloadDeserializationInputBuilder where
    pure a = TestHttpPayloadDeserializationInputBuilder (\s -> (s, a))
    (TestHttpPayloadDeserializationInputBuilder f) <*> (TestHttpPayloadDeserializationInputBuilder g) = TestHttpPayloadDeserializationInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpPayloadDeserializationInputBuilder where
    (TestHttpPayloadDeserializationInputBuilder f) >>= g = TestHttpPayloadDeserializationInputBuilder (\s ->
        let (s', a) = f s
            (TestHttpPayloadDeserializationInputBuilder h) = g a
        in h s')

setCoffeetype :: Data.Maybe.Maybe Data.Text.Text -> TestHttpPayloadDeserializationInputBuilder ()
setCoffeetype value =
   TestHttpPayloadDeserializationInputBuilder (\s -> (s { coffeeTypeBuilderState = value }, ()))

build :: TestHttpPayloadDeserializationInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpPayloadDeserializationInput
build builder = do
    let (st, _) = runTestHttpPayloadDeserializationInputBuilder builder defaultBuilderState
    coffeeType' <- Data.Either.Right (coffeeTypeBuilderState st)
    Data.Either.Right (TestHttpPayloadDeserializationInput { 
        coffeeType = coffeeType'
    })


