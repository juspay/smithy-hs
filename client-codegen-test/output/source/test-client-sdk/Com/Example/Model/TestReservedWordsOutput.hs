module Com.Example.Model.TestReservedWordsOutput (
    setType',
    setData',
    setAs',
    setCase',
    setClass',
    setDefault',
    setDeriving',
    setDo',
    setElse',
    setHiding',
    setIf',
    setImport',
    setIn',
    setInfix',
    setInfixl',
    setInfixr',
    setInstance',
    setLet',
    setModule',
    setNewtype',
    setOf',
    setQualified',
    setThen',
    setWhere',
    build,
    TestReservedWordsOutputBuilder,
    TestReservedWordsOutput,
    type',
    data',
    as',
    case',
    class',
    default',
    deriving',
    do',
    else',
    hiding',
    if',
    import',
    in',
    infix',
    infixl',
    infixr',
    instance',
    let',
    module',
    newtype',
    of',
    qualified',
    then',
    where'
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
import qualified Network.HTTP.Types

data TestReservedWordsOutput = TestReservedWordsOutput {
    type' :: Data.Text.Text,
    data' :: Data.Text.Text,
    as' :: Data.Text.Text,
    case' :: Data.Text.Text,
    class' :: Data.Text.Text,
    default' :: Data.Text.Text,
    deriving' :: Data.Text.Text,
    do' :: Data.Text.Text,
    else' :: Data.Text.Text,
    hiding' :: Data.Text.Text,
    if' :: Data.Text.Text,
    import' :: Data.Text.Text,
    in' :: Data.Text.Text,
    infix' :: Data.Text.Text,
    infixl' :: Data.Text.Text,
    infixr' :: Data.Text.Text,
    instance' :: Data.Text.Text,
    let' :: Data.Text.Text,
    module' :: Data.Text.Text,
    newtype' :: Data.Text.Text,
    of' :: Data.Text.Text,
    qualified' :: Data.Text.Text,
    then' :: Data.Text.Text,
    where' :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestReservedWordsOutput where
    toJSON a = Data.Aeson.object [
        "type" Data.Aeson..= type' a,
        "data" Data.Aeson..= data' a,
        "as" Data.Aeson..= as' a,
        "case" Data.Aeson..= case' a,
        "class" Data.Aeson..= class' a,
        "default" Data.Aeson..= default' a,
        "deriving" Data.Aeson..= deriving' a,
        "do" Data.Aeson..= do' a,
        "else" Data.Aeson..= else' a,
        "hiding" Data.Aeson..= hiding' a,
        "if" Data.Aeson..= if' a,
        "import" Data.Aeson..= import' a,
        "in" Data.Aeson..= in' a,
        "infix" Data.Aeson..= infix' a,
        "infixl" Data.Aeson..= infixl' a,
        "infixr" Data.Aeson..= infixr' a,
        "instance" Data.Aeson..= instance' a,
        "let" Data.Aeson..= let' a,
        "module" Data.Aeson..= module' a,
        "newtype" Data.Aeson..= newtype' a,
        "of" Data.Aeson..= of' a,
        "qualified" Data.Aeson..= qualified' a,
        "then" Data.Aeson..= then' a,
        "where" Data.Aeson..= where' a
        ]
    

instance Com.Example.Utility.SerializeBody TestReservedWordsOutput

instance Data.Aeson.FromJSON TestReservedWordsOutput where
    parseJSON = Data.Aeson.withObject "TestReservedWordsOutput" $ \v -> TestReservedWordsOutput
        Data.Functor.<$> (v Data.Aeson..: "type")
        Control.Applicative.<*> (v Data.Aeson..: "data")
        Control.Applicative.<*> (v Data.Aeson..: "as")
        Control.Applicative.<*> (v Data.Aeson..: "case")
        Control.Applicative.<*> (v Data.Aeson..: "class")
        Control.Applicative.<*> (v Data.Aeson..: "default")
        Control.Applicative.<*> (v Data.Aeson..: "deriving")
        Control.Applicative.<*> (v Data.Aeson..: "do")
        Control.Applicative.<*> (v Data.Aeson..: "else")
        Control.Applicative.<*> (v Data.Aeson..: "hiding")
        Control.Applicative.<*> (v Data.Aeson..: "if")
        Control.Applicative.<*> (v Data.Aeson..: "import")
        Control.Applicative.<*> (v Data.Aeson..: "in")
        Control.Applicative.<*> (v Data.Aeson..: "infix")
        Control.Applicative.<*> (v Data.Aeson..: "infixl")
        Control.Applicative.<*> (v Data.Aeson..: "infixr")
        Control.Applicative.<*> (v Data.Aeson..: "instance")
        Control.Applicative.<*> (v Data.Aeson..: "let")
        Control.Applicative.<*> (v Data.Aeson..: "module")
        Control.Applicative.<*> (v Data.Aeson..: "newtype")
        Control.Applicative.<*> (v Data.Aeson..: "of")
        Control.Applicative.<*> (v Data.Aeson..: "qualified")
        Control.Applicative.<*> (v Data.Aeson..: "then")
        Control.Applicative.<*> (v Data.Aeson..: "where")
    



data TestReservedWordsOutputBuilderState = TestReservedWordsOutputBuilderState {
    type'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    data'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    as'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    case'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    class'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    default'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    deriving'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    do'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    else'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    hiding'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    if'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    import'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    in'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    infix'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    infixl'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    infixr'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    instance'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    let'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    module'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    newtype'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    of'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    qualified'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    then'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    where'BuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestReservedWordsOutputBuilderState
defaultBuilderState = TestReservedWordsOutputBuilderState {
    type'BuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing,
    as'BuilderState = Data.Maybe.Nothing,
    case'BuilderState = Data.Maybe.Nothing,
    class'BuilderState = Data.Maybe.Nothing,
    default'BuilderState = Data.Maybe.Nothing,
    deriving'BuilderState = Data.Maybe.Nothing,
    do'BuilderState = Data.Maybe.Nothing,
    else'BuilderState = Data.Maybe.Nothing,
    hiding'BuilderState = Data.Maybe.Nothing,
    if'BuilderState = Data.Maybe.Nothing,
    import'BuilderState = Data.Maybe.Nothing,
    in'BuilderState = Data.Maybe.Nothing,
    infix'BuilderState = Data.Maybe.Nothing,
    infixl'BuilderState = Data.Maybe.Nothing,
    infixr'BuilderState = Data.Maybe.Nothing,
    instance'BuilderState = Data.Maybe.Nothing,
    let'BuilderState = Data.Maybe.Nothing,
    module'BuilderState = Data.Maybe.Nothing,
    newtype'BuilderState = Data.Maybe.Nothing,
    of'BuilderState = Data.Maybe.Nothing,
    qualified'BuilderState = Data.Maybe.Nothing,
    then'BuilderState = Data.Maybe.Nothing,
    where'BuilderState = Data.Maybe.Nothing
}

type TestReservedWordsOutputBuilder = Control.Monad.State.Strict.State TestReservedWordsOutputBuilderState

setType' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setType' value =
   Control.Monad.State.Strict.modify (\s -> (s { type'BuilderState = Data.Maybe.Just value }))

setData' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setData' value =
   Control.Monad.State.Strict.modify (\s -> (s { data'BuilderState = Data.Maybe.Just value }))

setAs' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setAs' value =
   Control.Monad.State.Strict.modify (\s -> (s { as'BuilderState = Data.Maybe.Just value }))

setCase' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setCase' value =
   Control.Monad.State.Strict.modify (\s -> (s { case'BuilderState = Data.Maybe.Just value }))

setClass' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setClass' value =
   Control.Monad.State.Strict.modify (\s -> (s { class'BuilderState = Data.Maybe.Just value }))

setDefault' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setDefault' value =
   Control.Monad.State.Strict.modify (\s -> (s { default'BuilderState = Data.Maybe.Just value }))

setDeriving' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setDeriving' value =
   Control.Monad.State.Strict.modify (\s -> (s { deriving'BuilderState = Data.Maybe.Just value }))

setDo' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setDo' value =
   Control.Monad.State.Strict.modify (\s -> (s { do'BuilderState = Data.Maybe.Just value }))

setElse' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setElse' value =
   Control.Monad.State.Strict.modify (\s -> (s { else'BuilderState = Data.Maybe.Just value }))

setHiding' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setHiding' value =
   Control.Monad.State.Strict.modify (\s -> (s { hiding'BuilderState = Data.Maybe.Just value }))

setIf' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setIf' value =
   Control.Monad.State.Strict.modify (\s -> (s { if'BuilderState = Data.Maybe.Just value }))

setImport' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setImport' value =
   Control.Monad.State.Strict.modify (\s -> (s { import'BuilderState = Data.Maybe.Just value }))

setIn' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setIn' value =
   Control.Monad.State.Strict.modify (\s -> (s { in'BuilderState = Data.Maybe.Just value }))

setInfix' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setInfix' value =
   Control.Monad.State.Strict.modify (\s -> (s { infix'BuilderState = Data.Maybe.Just value }))

setInfixl' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setInfixl' value =
   Control.Monad.State.Strict.modify (\s -> (s { infixl'BuilderState = Data.Maybe.Just value }))

setInfixr' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setInfixr' value =
   Control.Monad.State.Strict.modify (\s -> (s { infixr'BuilderState = Data.Maybe.Just value }))

setInstance' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setInstance' value =
   Control.Monad.State.Strict.modify (\s -> (s { instance'BuilderState = Data.Maybe.Just value }))

setLet' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setLet' value =
   Control.Monad.State.Strict.modify (\s -> (s { let'BuilderState = Data.Maybe.Just value }))

setModule' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setModule' value =
   Control.Monad.State.Strict.modify (\s -> (s { module'BuilderState = Data.Maybe.Just value }))

setNewtype' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setNewtype' value =
   Control.Monad.State.Strict.modify (\s -> (s { newtype'BuilderState = Data.Maybe.Just value }))

setOf' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setOf' value =
   Control.Monad.State.Strict.modify (\s -> (s { of'BuilderState = Data.Maybe.Just value }))

setQualified' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setQualified' value =
   Control.Monad.State.Strict.modify (\s -> (s { qualified'BuilderState = Data.Maybe.Just value }))

setThen' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setThen' value =
   Control.Monad.State.Strict.modify (\s -> (s { then'BuilderState = Data.Maybe.Just value }))

setWhere' :: Data.Text.Text -> TestReservedWordsOutputBuilder ()
setWhere' value =
   Control.Monad.State.Strict.modify (\s -> (s { where'BuilderState = Data.Maybe.Just value }))

build :: TestReservedWordsOutputBuilder () -> Data.Either.Either Data.Text.Text TestReservedWordsOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    type'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.type' is a required property.") Data.Either.Right (type'BuilderState st)
    data'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    as'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.as' is a required property.") Data.Either.Right (as'BuilderState st)
    case'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.case' is a required property.") Data.Either.Right (case'BuilderState st)
    class'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.class' is a required property.") Data.Either.Right (class'BuilderState st)
    default'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.default' is a required property.") Data.Either.Right (default'BuilderState st)
    deriving'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.deriving' is a required property.") Data.Either.Right (deriving'BuilderState st)
    do'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.do' is a required property.") Data.Either.Right (do'BuilderState st)
    else'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.else' is a required property.") Data.Either.Right (else'BuilderState st)
    hiding'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.hiding' is a required property.") Data.Either.Right (hiding'BuilderState st)
    if'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.if' is a required property.") Data.Either.Right (if'BuilderState st)
    import'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.import' is a required property.") Data.Either.Right (import'BuilderState st)
    in'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.in' is a required property.") Data.Either.Right (in'BuilderState st)
    infix'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.infix' is a required property.") Data.Either.Right (infix'BuilderState st)
    infixl'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.infixl' is a required property.") Data.Either.Right (infixl'BuilderState st)
    infixr'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.infixr' is a required property.") Data.Either.Right (infixr'BuilderState st)
    instance'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.instance' is a required property.") Data.Either.Right (instance'BuilderState st)
    let'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.let' is a required property.") Data.Either.Right (let'BuilderState st)
    module'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.module' is a required property.") Data.Either.Right (module'BuilderState st)
    newtype'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.newtype' is a required property.") Data.Either.Right (newtype'BuilderState st)
    of'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.of' is a required property.") Data.Either.Right (of'BuilderState st)
    qualified'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.qualified' is a required property.") Data.Either.Right (qualified'BuilderState st)
    then'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.then' is a required property.") Data.Either.Right (then'BuilderState st)
    where'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput.where' is a required property.") Data.Either.Right (where'BuilderState st)
    Data.Either.Right (TestReservedWordsOutput { 
        type' = type'',
        data' = data'',
        as' = as'',
        case' = case'',
        class' = class'',
        default' = default'',
        deriving' = deriving'',
        do' = do'',
        else' = else'',
        hiding' = hiding'',
        if' = if'',
        import' = import'',
        in' = in'',
        infix' = infix'',
        infixl' = infixl'',
        infixr' = infixr'',
        instance' = instance'',
        let' = let'',
        module' = module'',
        newtype' = newtype'',
        of' = of'',
        qualified' = qualified'',
        then' = then'',
        where' = where''
    })


instance Com.Example.Utility.FromResponseParser TestReservedWordsOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Com.Example.Utility.deSerField "qualified"
        var1 <- Com.Example.Utility.deSerField "instance"
        var2 <- Com.Example.Utility.deSerField "data"
        var3 <- Com.Example.Utility.deSerField "import"
        var4 <- Com.Example.Utility.deSerField "in"
        var5 <- Com.Example.Utility.deSerField "module"
        var6 <- Com.Example.Utility.deSerField "infixr"
        var7 <- Com.Example.Utility.deSerField "do"
        var8 <- Com.Example.Utility.deSerField "infix"
        var9 <- Com.Example.Utility.deSerField "then"
        var10 <- Com.Example.Utility.deSerField "type"
        var11 <- Com.Example.Utility.deSerField "newtype"
        var12 <- Com.Example.Utility.deSerField "hiding"
        var13 <- Com.Example.Utility.deSerField "as"
        var14 <- Com.Example.Utility.deSerField "default"
        var15 <- Com.Example.Utility.deSerField "deriving"
        var16 <- Com.Example.Utility.deSerField "else"
        var17 <- Com.Example.Utility.deSerField "infixl"
        var18 <- Com.Example.Utility.deSerField "of"
        var19 <- Com.Example.Utility.deSerField "let"
        var20 <- Com.Example.Utility.deSerField "where"
        var21 <- Com.Example.Utility.deSerField "class"
        var22 <- Com.Example.Utility.deSerField "if"
        var23 <- Com.Example.Utility.deSerField "case"
        pure $ TestReservedWordsOutput {
            type' = var10,
            data' = var2,
            as' = var13,
            case' = var23,
            class' = var21,
            default' = var14,
            deriving' = var15,
            do' = var7,
            else' = var16,
            hiding' = var12,
            if' = var22,
            import' = var3,
            in' = var4,
            infix' = var8,
            infixl' = var17,
            infixr' = var6,
            instance' = var1,
            let' = var19,
            module' = var5,
            newtype' = var11,
            of' = var18,
            qualified' = var0,
            then' = var9,
            where' = var20
        }

