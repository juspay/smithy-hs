module Com.Example.Model.TestHttpLabelsOutput (
    build,
    TestHttpLabelsOutputBuilder,
    TestHttpLabelsOutput
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpLabelsOutput = TestHttpLabelsOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpLabelsOutput where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON TestHttpLabelsOutput where
    parseJSON = Data.Aeson.withObject "TestHttpLabelsOutput" $ \_ -> pure $ TestHttpLabelsOutput



data TestHttpLabelsOutputBuilderState = TestHttpLabelsOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpLabelsOutputBuilderState
defaultBuilderState = TestHttpLabelsOutputBuilderState {
}

newtype TestHttpLabelsOutputBuilder a = TestHttpLabelsOutputBuilder {
    runTestHttpLabelsOutputBuilder :: TestHttpLabelsOutputBuilderState -> (TestHttpLabelsOutputBuilderState, a)
}

instance Data.Functor.Functor TestHttpLabelsOutputBuilder where
    fmap f (TestHttpLabelsOutputBuilder g) =
        TestHttpLabelsOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpLabelsOutputBuilder where
    pure a = TestHttpLabelsOutputBuilder (\s -> (s, a))
    (TestHttpLabelsOutputBuilder f) <*> (TestHttpLabelsOutputBuilder g) = TestHttpLabelsOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpLabelsOutputBuilder where
    (TestHttpLabelsOutputBuilder f) >>= g = TestHttpLabelsOutputBuilder (\s ->
        let (s', a) = f s
            (TestHttpLabelsOutputBuilder h) = g a
        in h s')


build :: TestHttpLabelsOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpLabelsOutput
build builder = do
    let (st, _) = runTestHttpLabelsOutputBuilder builder defaultBuilderState
    Data.Either.Right (TestHttpLabelsOutput { 
    })


