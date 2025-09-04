module Com.Example.Model.CoffeeItem (
    setCoffeetype,
    setDescription,
    setCreatedat,
    setUtc,
    setPosix,
    build,
    CoffeeItemBuilder,
    CoffeeItem,
    coffeeType,
    description,
    createdAt,
    utc,
    posix
) where
import qualified Com.Example.Model.CoffeeType
import qualified Com.Example.Utility
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified Data.Time.Clock.POSIX
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Date

data CoffeeItem = CoffeeItem {
    coffeeType :: Com.Example.Model.CoffeeType.CoffeeType,
    description :: Data.Text.Text,
    createdAt :: Network.HTTP.Date.HTTPDate,
    utc :: Data.Maybe.Maybe Data.Time.UTCTime,
    posix :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CoffeeItem where
    toJSON a = Data.Aeson.object [
        "type" Data.Aeson..= coffeeType a,
        "description" Data.Aeson..= description a,
        "createdAt" Data.Aeson..= ((createdAt a) Data.Function.& (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate)),
        "utc" Data.Aeson..= utc a,
        "posix" Data.Aeson..= posix a
        ]
    

instance Com.Example.Utility.SerializeBody CoffeeItem

instance Data.Aeson.FromJSON CoffeeItem where
    parseJSON = Data.Aeson.withObject "CoffeeItem" $ \v -> CoffeeItem
        Data.Functor.<$> (v Data.Aeson..: "type")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "createdAt"
             >>= \t -> t
                            Data.Function.& Data.Text.Encoding.encodeUtf8
                            Data.Function.& Network.HTTP.Date.parseHTTPDate
                            Data.Function.& Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.CoffeeItem.CoffeeItem.Network.HTTP.Date.HTTPDate as Network.HTTP.Date.HTTPDate") pure
            
            )
        Control.Applicative.<*> (v Data.Aeson..: "utc")
        Control.Applicative.<*> (v Data.Aeson..: "posix")
    



data CoffeeItemBuilderState = CoffeeItemBuilderState {
    coffeeTypeBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeType.CoffeeType,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    createdAtBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    utcBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    posixBuilderState :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CoffeeItemBuilderState
defaultBuilderState = CoffeeItemBuilderState {
    coffeeTypeBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    createdAtBuilderState = Data.Maybe.Nothing,
    utcBuilderState = Data.Maybe.Nothing,
    posixBuilderState = Data.Maybe.Nothing
}

type CoffeeItemBuilder = Control.Monad.State.Strict.State CoffeeItemBuilderState

setCoffeetype :: Com.Example.Model.CoffeeType.CoffeeType -> CoffeeItemBuilder ()
setCoffeetype value =
   Control.Monad.State.Strict.modify (\s -> (s { coffeeTypeBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> CoffeeItemBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setCreatedat :: Network.HTTP.Date.HTTPDate -> CoffeeItemBuilder ()
setCreatedat value =
   Control.Monad.State.Strict.modify (\s -> (s { createdAtBuilderState = Data.Maybe.Just value }))

setUtc :: Data.Maybe.Maybe Data.Time.UTCTime -> CoffeeItemBuilder ()
setUtc value =
   Control.Monad.State.Strict.modify (\s -> (s { utcBuilderState = value }))

setPosix :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime -> CoffeeItemBuilder ()
setPosix value =
   Control.Monad.State.Strict.modify (\s -> (s { posixBuilderState = value }))

build :: CoffeeItemBuilder () -> Data.Either.Either Data.Text.Text CoffeeItem
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    coffeeType' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.CoffeeItem.CoffeeItem.coffeeType is a required property.") Data.Either.Right (coffeeTypeBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.CoffeeItem.CoffeeItem.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    createdAt' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.CoffeeItem.CoffeeItem.createdAt is a required property.") Data.Either.Right (createdAtBuilderState st)
    utc' <- Data.Either.Right (utcBuilderState st)
    posix' <- Data.Either.Right (posixBuilderState st)
    Data.Either.Right (CoffeeItem { 
        coffeeType = coffeeType',
        description = description',
        createdAt = createdAt',
        utc = utc',
        posix = posix'
    })


