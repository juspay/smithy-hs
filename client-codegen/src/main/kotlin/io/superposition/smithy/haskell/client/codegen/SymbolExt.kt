package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import kotlin.jvm.optionals.getOrDefault

fun Symbol.isPrimitive(): Boolean = this.getProperty(SymbolProperties.IS_PRIMITIVE).getOrDefault(false)

fun Symbol.wrap(sym: Symbol) = sym.toBuilder().addReference(this).build()

fun Symbol.toMaybe() = this.wrap(HaskellSymbol.Maybe)

fun Symbol.toEither(right: Symbol) = right.wrap(HaskellSymbol.Either)
    .toBuilder()
    .addReference(this)
    .build()

fun Symbol.inIO() = this.wrap(HaskellSymbol.IO)
