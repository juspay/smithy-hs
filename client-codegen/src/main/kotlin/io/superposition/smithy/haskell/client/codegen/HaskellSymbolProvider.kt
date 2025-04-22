package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.shapes.Shape

class HaskellSymbolProvider(
        val model: Model,
        val serviceShape: ServiceShape,
        val pkgName: String
) : SymbolProvider {
    override fun toSymbol(shape: Shape): Symbol {
        return Symbol.Builder().build()
    }
}
