package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Property

object SymbolProperties {
    val IS_PRIMITIVE: Property<Boolean> = Property.named("is-primitive")
}
