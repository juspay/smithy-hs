package io.superposition.smithy.haskell.codegen

import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.shapes.Shape

public class HaskellSymbolProvider : SymbolProvider {
    override fun toSymbol(shape: Shape): Symbol {
        // TODO Implement.
        return Symbol.builder().build()
    }
}
