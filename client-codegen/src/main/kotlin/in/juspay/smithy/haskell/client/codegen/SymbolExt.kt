@file:Suppress("MaxLineLength")

package `in`.juspay.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolReference
import kotlin.jvm.optionals.getOrDefault

fun Symbol.isPrimitive(): Boolean = this.getProperty(SymbolProperties.IS_PRIMITIVE).getOrDefault(
    false
)

val Symbol.isWrapper: Boolean
    get() = this.getProperty(SymbolProperties.WRAPPER_T).getOrDefault(false)

fun Symbol.wrap(sym: Symbol) = sym.toBuilder()
    .addReference(this)
    .putProperty(SymbolProperties.WRAPPER_T, true)
    .build()

fun Symbol.isMaybe() =
    this.name == HaskellSymbol.Maybe.name &&
        this.namespace == HaskellSymbol.Maybe.namespace

fun Symbol.isEither() =
    this.name == HaskellSymbol.Either.name &&
        this.namespace == HaskellSymbol.Either.namespace

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

fun Symbol.isOrWrapped(other: Symbol): Boolean {
    if (this == other) {
        return true
    }
    if (this.isEither()) {
        // Checks against the `Right` reference.
        return this.references[1].symbol == other
    }
    return this.isWrapper && this.references.first().symbol == other
}

fun SymbolReference.isDeclare() =
    this.options.any { it == SymbolReference.ContextOption.DECLARE }

fun SymbolReference.isUse() = this.options.any { it == SymbolReference.ContextOption.USE }
