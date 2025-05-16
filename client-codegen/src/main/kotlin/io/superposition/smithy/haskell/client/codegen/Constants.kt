package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Property
import software.amazon.smithy.codegen.core.Symbol

object SymbolProperties {
    val IS_PRIMITIVE: Property<Boolean> = Property.named("is-primitive")
    val SERIALIZABLE: Property<Boolean> = Property.named("serializable")
    val DESERIALIZABLE: Property<Boolean> = Property.named("deserializable")
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
    val Eq: Symbol = Symbol.builder().name("Eq").build()
    val Generic: Symbol = Symbol.builder().name("Generic").namespace("GHC.Generics", ".").build()
    val FromJSON: Symbol = Symbol.builder().name("FromJSON").namespace("Data.Aeson", ".").build()
    val ToJSON: Symbol = Symbol.builder().name("ToJSON").namespace("Data.Aeson", ".").build()
    val JsonString: Symbol = Symbol.builder().name("String").namespace("Data.Aeson", ".").build()
    val TextPack: Symbol = Symbol.builder().name("pack").namespace("Data.Text", ".").build()
    val JsonObjectBuilder: Symbol = Symbol.builder().name("object").namespace("Data.Aeson", ".").build()
    val ByteString: Symbol = Symbol.builder()
        .name("ByteString")
        .namespace("Data.ByteString.Lazy", ".")
        .build()
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
}
