module Com.Example.Model.TestReservedWordsInput (
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
    TestReservedWordsInputBuilder,
    TestReservedWordsInput,
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

data TestReservedWordsInput = TestReservedWordsInput {
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

instance Data.Aeson.ToJSON TestReservedWordsInput where
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
    


instance Data.Aeson.FromJSON TestReservedWordsInput where
    parseJSON = Data.Aeson.withObject "TestReservedWordsInput" $ \v -> TestReservedWordsInput
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
    



data TestReservedWordsInputBuilderState = TestReservedWordsInputBuilderState {
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

defaultBuilderState :: TestReservedWordsInputBuilderState
defaultBuilderState = TestReservedWordsInputBuilderState {
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

newtype TestReservedWordsInputBuilder a = TestReservedWordsInputBuilder {
    runTestReservedWordsInputBuilder :: TestReservedWordsInputBuilderState -> (TestReservedWordsInputBuilderState, a)
}

instance Data.Functor.Functor TestReservedWordsInputBuilder where
    fmap f (TestReservedWordsInputBuilder g) =
        TestReservedWordsInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestReservedWordsInputBuilder where
    pure a = TestReservedWordsInputBuilder (\s -> (s, a))
    (TestReservedWordsInputBuilder f) <*> (TestReservedWordsInputBuilder g) = TestReservedWordsInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestReservedWordsInputBuilder where
    (TestReservedWordsInputBuilder f) >>= g = TestReservedWordsInputBuilder (\s ->
        let (s', a) = f s
            (TestReservedWordsInputBuilder h) = g a
        in h s')

setType' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setType' value =
   TestReservedWordsInputBuilder (\s -> (s { type'BuilderState = Data.Maybe.Just value }, ()))

setData' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setData' value =
   TestReservedWordsInputBuilder (\s -> (s { data'BuilderState = Data.Maybe.Just value }, ()))

setAs' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setAs' value =
   TestReservedWordsInputBuilder (\s -> (s { as'BuilderState = Data.Maybe.Just value }, ()))

setCase' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setCase' value =
   TestReservedWordsInputBuilder (\s -> (s { case'BuilderState = Data.Maybe.Just value }, ()))

setClass' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setClass' value =
   TestReservedWordsInputBuilder (\s -> (s { class'BuilderState = Data.Maybe.Just value }, ()))

setDefault' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setDefault' value =
   TestReservedWordsInputBuilder (\s -> (s { default'BuilderState = Data.Maybe.Just value }, ()))

setDeriving' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setDeriving' value =
   TestReservedWordsInputBuilder (\s -> (s { deriving'BuilderState = Data.Maybe.Just value }, ()))

setDo' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setDo' value =
   TestReservedWordsInputBuilder (\s -> (s { do'BuilderState = Data.Maybe.Just value }, ()))

setElse' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setElse' value =
   TestReservedWordsInputBuilder (\s -> (s { else'BuilderState = Data.Maybe.Just value }, ()))

setHiding' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setHiding' value =
   TestReservedWordsInputBuilder (\s -> (s { hiding'BuilderState = Data.Maybe.Just value }, ()))

setIf' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setIf' value =
   TestReservedWordsInputBuilder (\s -> (s { if'BuilderState = Data.Maybe.Just value }, ()))

setImport' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setImport' value =
   TestReservedWordsInputBuilder (\s -> (s { import'BuilderState = Data.Maybe.Just value }, ()))

setIn' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setIn' value =
   TestReservedWordsInputBuilder (\s -> (s { in'BuilderState = Data.Maybe.Just value }, ()))

setInfix' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setInfix' value =
   TestReservedWordsInputBuilder (\s -> (s { infix'BuilderState = Data.Maybe.Just value }, ()))

setInfixl' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setInfixl' value =
   TestReservedWordsInputBuilder (\s -> (s { infixl'BuilderState = Data.Maybe.Just value }, ()))

setInfixr' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setInfixr' value =
   TestReservedWordsInputBuilder (\s -> (s { infixr'BuilderState = Data.Maybe.Just value }, ()))

setInstance' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setInstance' value =
   TestReservedWordsInputBuilder (\s -> (s { instance'BuilderState = Data.Maybe.Just value }, ()))

setLet' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setLet' value =
   TestReservedWordsInputBuilder (\s -> (s { let'BuilderState = Data.Maybe.Just value }, ()))

setModule' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setModule' value =
   TestReservedWordsInputBuilder (\s -> (s { module'BuilderState = Data.Maybe.Just value }, ()))

setNewtype' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setNewtype' value =
   TestReservedWordsInputBuilder (\s -> (s { newtype'BuilderState = Data.Maybe.Just value }, ()))

setOf' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setOf' value =
   TestReservedWordsInputBuilder (\s -> (s { of'BuilderState = Data.Maybe.Just value }, ()))

setQualified' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setQualified' value =
   TestReservedWordsInputBuilder (\s -> (s { qualified'BuilderState = Data.Maybe.Just value }, ()))

setThen' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setThen' value =
   TestReservedWordsInputBuilder (\s -> (s { then'BuilderState = Data.Maybe.Just value }, ()))

setWhere' :: Data.Text.Text -> TestReservedWordsInputBuilder ()
setWhere' value =
   TestReservedWordsInputBuilder (\s -> (s { where'BuilderState = Data.Maybe.Just value }, ()))

build :: TestReservedWordsInputBuilder () -> Data.Either.Either Data.Text.Text TestReservedWordsInput
build builder = do
    let (st, _) = runTestReservedWordsInputBuilder builder defaultBuilderState
    type'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.type' is a required property.") Data.Either.Right (type'BuilderState st)
    data'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    as'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.as' is a required property.") Data.Either.Right (as'BuilderState st)
    case'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.case' is a required property.") Data.Either.Right (case'BuilderState st)
    class'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.class' is a required property.") Data.Either.Right (class'BuilderState st)
    default'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.default' is a required property.") Data.Either.Right (default'BuilderState st)
    deriving'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.deriving' is a required property.") Data.Either.Right (deriving'BuilderState st)
    do'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.do' is a required property.") Data.Either.Right (do'BuilderState st)
    else'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.else' is a required property.") Data.Either.Right (else'BuilderState st)
    hiding'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.hiding' is a required property.") Data.Either.Right (hiding'BuilderState st)
    if'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.if' is a required property.") Data.Either.Right (if'BuilderState st)
    import'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.import' is a required property.") Data.Either.Right (import'BuilderState st)
    in'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.in' is a required property.") Data.Either.Right (in'BuilderState st)
    infix'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.infix' is a required property.") Data.Either.Right (infix'BuilderState st)
    infixl'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.infixl' is a required property.") Data.Either.Right (infixl'BuilderState st)
    infixr'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.infixr' is a required property.") Data.Either.Right (infixr'BuilderState st)
    instance'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.instance' is a required property.") Data.Either.Right (instance'BuilderState st)
    let'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.let' is a required property.") Data.Either.Right (let'BuilderState st)
    module'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.module' is a required property.") Data.Either.Right (module'BuilderState st)
    newtype'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.newtype' is a required property.") Data.Either.Right (newtype'BuilderState st)
    of'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.of' is a required property.") Data.Either.Right (of'BuilderState st)
    qualified'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.qualified' is a required property.") Data.Either.Right (qualified'BuilderState st)
    then'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.then' is a required property.") Data.Either.Right (then'BuilderState st)
    where'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput.where' is a required property.") Data.Either.Right (where'BuilderState st)
    Data.Either.Right (TestReservedWordsInput { 
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


