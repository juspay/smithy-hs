module Com.Example.Model.CoffeeItem (
    CoffeeItem,
    ctype,
    description,
    setCtype,
    setDescription,
    build,
    CoffeeItemBuilder
) where

import qualified Com.Example.Model.CoffeeType
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text

data CoffeeItem = CoffeeItem {
    ctype :: Com.Example.Model.CoffeeType.CoffeeType,
    description :: Data.Text.Text
}

data CoffeeItemBuilderState = CoffeeItemBuilderState {
    ctypeBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeType.CoffeeType,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text
}

defaultBuilderState :: CoffeeItemBuilderState
defaultBuilderState = CoffeeItemBuilderState {
    ctypeBuilderState = Data.Maybe.Nothing,
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

setCtype :: Com.Example.Model.CoffeeType.CoffeeType -> CoffeeItemBuilder ()
setCtype value =
   CoffeeItemBuilder (\s -> (s { ctypeBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> CoffeeItemBuilder ()
setDescription value =
   CoffeeItemBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

build :: CoffeeItemBuilder () -> Data.Either.Either Data.Text.Text CoffeeItem
build builder = do
    let (st, _) = runCoffeeItemBuilder builder defaultBuilderState
    ctype' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.CoffeeItem.CoffeeItem.ctype is a required property.") Data.Either.Right (ctypeBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.CoffeeItem.CoffeeItem.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    Data.Either.Right (CoffeeItem { 
        ctype = ctype',
        description = description'
    })


