{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.GetMenuInput (
    build,
    GetMenuInputBuilder,
    GetMenuInput
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics

data GetMenuInput = GetMenuInput {
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetMenuInput where
    toJSON a = Data.Aeson.object
        []



data GetMenuInputBuilderState = GetMenuInputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetMenuInputBuilderState
defaultBuilderState = GetMenuInputBuilderState {
}

newtype GetMenuInputBuilder a = GetMenuInputBuilder {
    runGetMenuInputBuilder :: GetMenuInputBuilderState -> (GetMenuInputBuilderState, a)
}

instance Data.Functor.Functor GetMenuInputBuilder where
    fmap f (GetMenuInputBuilder g) =
        GetMenuInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetMenuInputBuilder where
    pure a = GetMenuInputBuilder (\s -> (s, a))
    (GetMenuInputBuilder f) <*> (GetMenuInputBuilder g) = GetMenuInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetMenuInputBuilder where
    (GetMenuInputBuilder f) >>= g = GetMenuInputBuilder (\s ->
        let (s', a) = f s
            (GetMenuInputBuilder h) = g a
        in h s')


build :: GetMenuInputBuilder () -> Data.Either.Either Data.Text.Text GetMenuInput
build builder = do
    let (st, _) = runGetMenuInputBuilder builder defaultBuilderState
    Data.Either.Right (GetMenuInput { 
    })


