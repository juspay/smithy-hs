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
}

object HaskellSymbol {
    val IO: Symbol = Symbol.builder()
        .name("IO")
        .namespace("Control.Monad.IO", ".")
        .build()
    val MonadIO: Symbol = Symbol.builder()
        .name("MonadIO")
        .namespace("Control.Monad.IO.Class", ".")
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

    val Eq: Symbol = Symbol.builder().name("Eq").build()

    val FromJSON: Symbol = Symbol.builder().name("FromJSON").namespace("Data.Aeson", ".").build()

    val ToJSON: Symbol = Symbol.builder().name("ToJSON").namespace("Data.Aeson", ".").build()

    val JsonString: Symbol = Symbol.builder().name("String").namespace("Data.Aeson", ".").build()

    val TextPack: Symbol = Symbol.builder().name("pack").namespace("Data.Text", ".").build()
    val JsonObjectBuilder: Symbol = Symbol.builder().name("object").namespace("Data.Aeson", ".").build()
    val ByteString: Symbol = Symbol.builder().name("ByteString").namespace("Data.ByteString.Lazy", ".").build()
    val JsonEncode: Symbol = Symbol.builder()
        .name("encode")
        .namespace("Data.Aeson", ".")
        .build()
    val HaskellMap: Symbol = Symbol.builder().name("Map").namespace("Data.Map", ".").build()
    val QueryString: Symbol = Symbol.builder()
        .name("QueryString")
        .namespace("Network.HTTP", ".")
        .build()
    val ToQuery: Symbol = Symbol.builder()
        .name("toQuery")
        .namespace("Query", ".")
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
