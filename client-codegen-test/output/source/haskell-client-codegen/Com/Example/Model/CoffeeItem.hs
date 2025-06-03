{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.CoffeeItem (
    setCoffeetype,
    setDescription,
    build,
    CoffeeItemBuilder,
    CoffeeItem,
    coffeeType,
    description
) where
import qualified Com.Example.Model.CoffeeType
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data CoffeeItem = CoffeeItem {
    coffeeType :: Com.Example.Model.CoffeeType.CoffeeType,
    description :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CoffeeItem where
    toJSON a = Data.Aeson.object [
        "type" Data.Aeson..= coffeeType a,
        "description" Data.Aeson..= description a
        ]
    


instance Data.Aeson.FromJSON CoffeeItem where
    parseJSON = Data.Aeson.withObject "CoffeeItem" $ \v -> CoffeeItem
        Data.Functor.<$> (v Data.Aeson..: "type")
        Control.Applicative.<*> (v Data.Aeson..: "description")
    



data CoffeeItemBuilderState = CoffeeItemBuilderState {
    coffeeTypeBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeType.CoffeeType,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CoffeeItemBuilderState
defaultBuilderState = CoffeeItemBuilderState {
    coffeeTypeBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing
}

newtype CoffeeItemBuilder a = CoffeeItemBuilder {
    runCoffeeItemBuilder :: CoffeeItemBuilderState -> (CoffeeItemBuilderState, a)
}

instance Data.Functor.Functor CoffeeItemBuilder where
    fmap f (CoffeeItemBuilder g) =
        CoffeeItemBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CoffeeItemBuilder where
    pure a = CoffeeItemBuilder (\s -> (s, a))
    (CoffeeItemBuilder f) <*> (CoffeeItemBuilder g) = CoffeeItemBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CoffeeItemBuilder where
    (CoffeeItemBuilder f) >>= g = CoffeeItemBuilder (\s ->
        let (s', a) = f s
            (CoffeeItemBuilder h) = g a
        in h s')

setCoffeetype :: Com.Example.Model.CoffeeType.CoffeeType -> CoffeeItemBuilder ()
setCoffeetype value =
   CoffeeItemBuilder (\s -> (s { coffeeTypeBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> CoffeeItemBuilder ()
setDescription value =
   CoffeeItemBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

build :: CoffeeItemBuilder () -> Data.Either.Either Data.Text.Text CoffeeItem
build builder = do
    let (st, _) = runCoffeeItemBuilder builder defaultBuilderState
    coffeeType' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.CoffeeItem.CoffeeItem.coffeeType is a required property.") Data.Either.Right (coffeeTypeBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.CoffeeItem.CoffeeItem.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    Data.Either.Right (CoffeeItem { 
        coffeeType = coffeeType',
        description = description'
    })


