package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import kotlin.jvm.optionals.getOrDefault

fun Symbol.isPrimitive(): Boolean = this.getProperty(SymbolProperties.IS_PRIMITIVE).getOrDefault(false)

fun Symbol.wrap(sym: Symbol) = sym.toBuilder().addReference(this).build()
