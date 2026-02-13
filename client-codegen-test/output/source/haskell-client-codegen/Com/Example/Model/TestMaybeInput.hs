module Com.Example.Model.TestMaybeInput (
    setTestfield,
    setTestmaybefield,
    build,
    TestMaybeInputBuilder,
    TestMaybeInput,
    testField,
    testMaybeField
) where
import qualified Com.Example.Utility
import qualified Control.Applicative
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

data TestMaybeInput = TestMaybeInput {
    testField :: Data.Text.Text,
    testMaybeField :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestMaybeInput where
    toJSON a = Data.Aeson.object [
        "testField" Data.Aeson..= testField a,
        "testMaybeField" Data.Aeson..= testMaybeField a
        ]
    

instance Com.Example.Utility.SerializeBody TestMaybeInput

instance Data.Aeson.FromJSON TestMaybeInput where
    parseJSON = Data.Aeson.withObject "TestMaybeInput" $ \v -> TestMaybeInput
        Data.Functor.<$> (v Data.Aeson..: "testField")
        Control.Applicative.<*> (v Data.Aeson..:? "testMaybeField")
    



data TestMaybeInputBuilderState = TestMaybeInputBuilderState {
    testFieldBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    testMaybeFieldBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestMaybeInputBuilderState
defaultBuilderState = TestMaybeInputBuilderState {
    testFieldBuilderState = Data.Maybe.Nothing,
    testMaybeFieldBuilderState = Data.Maybe.Nothing
}

type TestMaybeInputBuilder = Control.Monad.State.Strict.State TestMaybeInputBuilderState

setTestfield :: Data.Text.Text -> TestMaybeInputBuilder ()
setTestfield value =
   Control.Monad.State.Strict.modify (\s -> (s { testFieldBuilderState = Data.Maybe.Just value }))

setTestmaybefield :: Data.Maybe.Maybe Data.Text.Text -> TestMaybeInputBuilder ()
setTestmaybefield value =
   Control.Monad.State.Strict.modify (\s -> (s { testMaybeFieldBuilderState = value }))

build :: TestMaybeInputBuilder () -> Data.Either.Either Data.Text.Text TestMaybeInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    testField' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestMaybeInput.TestMaybeInput.testField is a required property.") Data.Either.Right (testFieldBuilderState st)
    testMaybeField' <- Data.Either.Right (testMaybeFieldBuilderState st)
    Data.Either.Right (TestMaybeInput { 
        testField = testField',
        testMaybeField = testMaybeField'
    })


instance Com.Example.Utility.IntoRequestBuilder TestMaybeInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Com.Example.Utility.setPath [
            "test-maybe"
            ]
        
        
        Com.Example.Utility.serField "testField" (testField self)
        Com.Example.Utility.serField "testMaybeField" (testMaybeField self)

