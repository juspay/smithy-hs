module Com.Example.Model.Error400 (
    setMessage,
    build,
    Error400Builder,
    Error400,
    message
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
import qualified Network.HTTP.Types

data Error400 = Error400 {
    message :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON Error400 where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    

instance Com.Example.Utility.SerializeBody Error400

instance Data.Aeson.FromJSON Error400 where
    parseJSON = Data.Aeson.withObject "Error400" $ \v -> Error400
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data Error400BuilderState = Error400BuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: Error400BuilderState
defaultBuilderState = Error400BuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

type Error400Builder = Control.Monad.State.Strict.State Error400BuilderState

setMessage :: Data.Text.Text -> Error400Builder ()
setMessage value =
   Control.Monad.State.Strict.modify (\s -> (s { messageBuilderState = Data.Maybe.Just value }))

build :: Error400Builder () -> Data.Either.Either Data.Text.Text Error400
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.Error400.Error400.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (Error400 { 
        message = message'
    })


instance Com.Example.Utility.FromResponseParser Error400 where
    expectedStatus = Network.HTTP.Types.status400
    responseParser = do
        
        var0 <- Com.Example.Utility.deSerField "message"
        pure $ Error400 {
            message = var0
        }

