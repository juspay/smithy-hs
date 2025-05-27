package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Property
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolDependency

object SymbolProperties {
    val IS_PRIMITIVE: Property<Boolean> = Property.named("is-primitive")
}

object HaskellDependencies {
    val HttpClient = SymbolDependency.builder()
        .packageName("http-client")
        .version(CodegenUtils.depRange("0.5.14", "0.8"))
        .build()
    val NetworkUri = SymbolDependency.builder()
        .packageName("network-uri")
        .version(CodegenUtils.depRange("2.6", "2.7"))
        .build()
    val CaseInsensitive = SymbolDependency.builder()
        .packageName("case-insensitive")
        .version(CodegenUtils.depRange("1.2.1", "1.3"))
        .build()
}

object HaskellSymbol {
    object Misc {
        val CaseInsensitive: Symbol = Symbol.builder()
            .name("CI")
            .namespace("Data.CaseInsensitive", ".")
            .dependencies(HaskellDependencies.CaseInsensitive)
            .build()
    }

    val IO: Symbol = Symbol.builder()
        .name("IO")
        .build()
    val Maybe: Symbol = Symbol.builder()
        .name("Maybe")
        .namespace("Data.Maybe", ".")
        .build()
    val Either: Symbol = Symbol.builder()
        .name("Either")
        .namespace("Data.Either", ".")
        .build()
    val Functor = Symbol.builder()
        .name("Functor")
        .namespace("Data.Functor", ".")
        .build()
    val Applicative = Symbol.builder()
        .name("Applicative")
        .namespace("Control.Applicative", ".")
        .build()
    val Monad = Symbol.builder()
        .name("Monad")
        .namespace("Control.Monad", ".")
        .build()

    val Text = Symbol.builder()
        .name("Text")
        .namespace("Data.Text", ".")
        .dependencies(
            SymbolDependency.builder()
                .packageName("text")
                .version(CodegenUtils.depRange("1.2.3", "2.1"))
                .build()
        )
        .build()

    val Generic: Symbol = Symbol.builder()
        .name("Generic")
        .namespace("GHC.Generics", ".")
        .build()

    val Eq: Symbol = Symbol.builder().name("Eq").namespace("Data.Eq", ".").build()

    val Aeson: Symbol = Symbol.builder().name("Aeson").namespace("Data.Aeson", ".")
        .dependencies(
            SymbolDependency.builder()
                .packageName("aeson")
                .version(CodegenUtils.depRange("2.0.0", "2.2.0"))
                .build()
        )
        .build()
    val ParseEither = Symbol.builder().name("parseEither")
        .namespace("Data.Aeson.Types", ".")
        .build()
    val ToJSON: Symbol = Aeson.toBuilder().name("ToJSON")
        .build()
    val JsonString: Symbol = Aeson.toBuilder().name("String").build()
    val JsonObjectBuilder: Symbol = Aeson.toBuilder()
        .name("object")
        .build()

    val TextPack: Symbol =
        Symbol.builder().name("pack").namespace("Data.Text", ".").build()

    val EncodingUtf8: Symbol = Symbol.builder()
        .name("encodeUtf8")
        .namespace("Data.Text.Encoding", ".")
        .build()

    val ByteString: Symbol = Symbol.builder()
        .name("ByteString")
        .namespace("Data.ByteString", ".")
        .dependencies(
            SymbolDependency.builder()
                .packageName("bytestring")
                .version(CodegenUtils.depRange("0.10.12", "0.12.0"))
                .build()
        )
        .build()

    val LazyByteString: Symbol = Symbol.builder()
        .name("ByteString")
        .namespace("Data.ByteString.Lazy", ".")
        .dependencies(
            SymbolDependency.builder()
                .packageName("bytestring")
                .version(CodegenUtils.depRange("0.10.12", "0.12.0"))
                .build()
        )
        .build()

    val Map: Symbol = Symbol.builder().name("Map").namespace("Data.Map", ".")
        .dependencies(
            SymbolDependency.builder()
                .packageName("containers")
                .version(CodegenUtils.depRange("0.6.4", "0.7"))
                .build()
        )
        .build()

    val List: Symbol = Symbol.builder()
        .name("List")
        .namespace("Data.List", ".")
        .build()

    val Flip: Symbol = Symbol.builder()
        .name("&")
        .namespace("Data.Function", ".")
        .build()
    val And = Flip

    val SomeException: Symbol = Symbol.builder()
        .name("SomeException")
        .namespace("Control.Exception", ".")
        .build()

    object Http {
        val Manager = Symbol.builder()
            .name("Manager")
            .namespace("Network.HTTP.Client", ".")
            .dependencies(HaskellDependencies.HttpClient)
            .build()
        val ManagerSettings = Manager.toBuilder().name("ManagerSettings").build()
        val Uri = Symbol.builder()
            .name("URI")
            .namespace("Network.URI", ".")
            .dependencies(HaskellDependencies.NetworkUri)
            .build()
    }
}

object BiFunctor {
    val first = Symbol.builder()
        .name("first")
        .namespace("Data.Bifunctor", ".")
        .build()
}

object Http {
    private const val CLIENT_MODULE = "Network.HTTP.Client"
    private const val TYPES_MODULE = "Network.HTTP.Types"

    private val HttpTypesModule = Symbol.builder()
        .name("")
        .namespace(TYPES_MODULE, ".")
        .dependencies(
            SymbolDependency.builder()
                .packageName("http-types")
                .version(CodegenUtils.depRange("0.12.3", "0.13"))
                .build()
        )
        .build()
    private val HttpClientModule = Symbol.builder()
        .name("")
        .namespace(CLIENT_MODULE, ".")
        .dependencies(
            SymbolDependency.builder()
                .packageName("http-client")
                .version(CodegenUtils.depRange("0.7.9", "0.8"))
                .build()
        )
        .build()

    val Query: Symbol = HttpTypesModule.toBuilder()
        .name("Query")
        .namespace("$TYPES_MODULE.URI", ".")
        .build()

    val Post: Symbol = HttpTypesModule.toBuilder()
        .name("methodPost")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Get: Symbol = HttpTypesModule.toBuilder()
        .name("methodGet")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Put: Symbol = HttpTypesModule.toBuilder()
        .name("methodPut")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Head: Symbol = HttpTypesModule.toBuilder()
        .name("methodHead")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Delete: Symbol = HttpTypesModule.toBuilder()
        .name("methodDelete")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Patch: Symbol = HttpTypesModule.toBuilder()
        .name("methodPatch")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Options: Symbol = HttpTypesModule.toBuilder()
        .name("methodOptions")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Trace: Symbol = HttpTypesModule.toBuilder()
        .name("methodTrace")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Connect: Symbol = HttpTypesModule.toBuilder()
        .name("methodConnect")
        .namespace("$TYPES_MODULE.Method", ".")
        .build()
    val Custom: Symbol = Symbol.builder()
        .name("Custom")
        .build()

    val Request: Symbol = HttpClientModule.toBuilder()
        .name("Request")
        .namespace(CLIENT_MODULE, ".")
        .build()
    val rqPath: Symbol = HttpClientModule.toBuilder()
        .name("path")
        .namespace(CLIENT_MODULE, ".")
        .build()
    val rqMethod: Symbol = HttpClientModule.toBuilder()
        .name("method")
        .namespace(CLIENT_MODULE, ".")
        .build()
    val rqHeaders: Symbol = HttpClientModule.toBuilder()
        .name("requestHeaders")
        .namespace(CLIENT_MODULE, ".")
        .build()
    val rqBody: Symbol = HttpClientModule.toBuilder()
        .name("requestBody")
        .namespace(CLIENT_MODULE, ".")
        .build()
    val rqQueryString: Symbol = HttpClientModule.toBuilder()
        .name("queryString")
        .namespace(CLIENT_MODULE, ".")
        .build()

    val HttpClient: Symbol = HttpClientModule.toBuilder()
        .name("httpLbs")
        .namespace(CLIENT_MODULE, ".")
        .build()

    val DefaultHttpManagerSettings: Symbol = HttpClientModule.toBuilder()
        .name("defaultManagerSettings")
        .namespace(CLIENT_MODULE, ".")
        .build()

    val NewManager: Symbol = HttpClientModule.toBuilder()
        .name("newManager")
        .namespace(CLIENT_MODULE, ".")
        .build()
}
