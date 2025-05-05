package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol

fun Symbol.isPrimitive(): Boolean {
    return this.expectProperty(SymbolProperties.IS_PRIMITIVE)
}
