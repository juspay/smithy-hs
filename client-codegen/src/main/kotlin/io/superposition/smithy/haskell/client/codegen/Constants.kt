package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Property
import software.amazon.smithy.codegen.core.Symbol

object SymbolProperties {
    val IS_PRIMITIVE: Property<Boolean> = Property.named("is-primitive")
    val SERIALIZABLE: Property<Boolean> = Property.named("serializable")
    val DESERIALIZABLE: Property<Boolean> = Property.named("deserializable")
}

object HaskellSymbol {
    val IO = Symbol.builder()
        .name("IO")
        .namespace("Control.Monad.IO", ".")
        .build()
    val MonadIO = Symbol.builder()
        .name("MonadIO")
        .namespace("Control.Monad.IO.Class", ".")
        .build()
    val Maybe = Symbol.builder()
        .name("Maybe")
        .namespace("Data.Maybe", ".")
        .build()
    val Either = Symbol.builder()
        .name("Either")
        .namespace("Data.Either", ".")
        .build()
    val Eq = Symbol.builder().name("Eq").build()
    val Generic = Symbol.builder().name("Generic").namespace("GHC.Generics", ".").build()
    val FromJSON = Symbol.builder().name("FromJSON").namespace("Data.Aeson", ".").build()
    val ToJSON = Symbol.builder().name("ToJSON").namespace("Data.Aeson", ".").build()
    val JsonString = Symbol.builder().name("String").namespace("Data.Aeson", ".").build()
    val TextPack = Symbol.builder().name("pack").namespace("Data.Text", ".").build()
    val JsonObjectBuilder = Symbol.builder().name("object").namespace("Data.Aeson", ".").build()
    val ByteString = Symbol.builder().name("ByteString").namespace("Data.ByteString.Lazy", ".").build()
    val JsonEncode = Symbol.builder()
        .name("encode")
        .namespace("Data.Aeson", ".")
        .build()
}
