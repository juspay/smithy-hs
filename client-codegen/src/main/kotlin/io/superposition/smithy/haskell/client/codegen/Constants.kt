package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Property
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolDependency

object SymbolProperties {
    val IS_PRIMITIVE: Property<Boolean> = Property.named("is-primitive")
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
    val Functor = Symbol.builder()
        .name("Functor")
        .namespace("Data.Functor", ".")
        .build()
    val Applicative = Symbol.builder()
        .name("Applicative")
        .namespace("Data.Applicative", ".")
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
                .version(CodegenUtils.depRange("1.2.3", "2.0"))
                .build()
        )
        .build()
}
