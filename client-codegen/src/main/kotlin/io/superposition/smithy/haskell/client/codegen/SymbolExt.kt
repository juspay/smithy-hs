@file:Suppress("MaxLineLength")

package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolReference
import kotlin.jvm.optionals.getOrDefault

fun Symbol.isPrimitive(): Boolean = this.getProperty(SymbolProperties.IS_PRIMITIVE).getOrDefault(
    false
)

fun Symbol.wrap(sym: Symbol) = sym.toBuilder().addReference(this).build()

fun Symbol.isMaybe() =
    this.name == HaskellSymbol.Maybe.name &&
        this.namespace == HaskellSymbol.Maybe.namespace

fun Symbol.toMaybe(): Symbol {
    if (this.isMaybe()) {
        return this
    }
    return this.wrap(HaskellSymbol.Maybe)
}

fun Symbol.toEither(right: Symbol) = this.wrap(HaskellSymbol.Either)
    .toBuilder()
    .addReference(right)
    .build()

fun Symbol.inIO() = this.wrap(HaskellSymbol.IO)

fun SymbolReference.isDeclare() =
    this.options.any { it == SymbolReference.ContextOption.DECLARE }

fun SymbolReference.isUse() = this.options.any { it == SymbolReference.ContextOption.USE }
