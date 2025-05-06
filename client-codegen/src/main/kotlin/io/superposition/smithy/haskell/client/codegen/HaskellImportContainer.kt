package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.ImportContainer
import software.amazon.smithy.codegen.core.Symbol
import java.util.logging.Logger

class HaskellImportContainer(private val modName: String) : ImportContainer {
    private val imports: MutableMap<String, MutableSet<Symbol>> = HashMap()
    private val logger: Logger = Logger.getLogger(this.javaClass.name)

    override fun importSymbol(symbol: Symbol, alias: String?) {
        if (symbol.isPrimitive()) {
            return
        }
        val duplicates = imports.computeIfAbsent(symbol.name) { HashSet() }
        duplicates.add(symbol)
    }

    override fun toString(): String {
        val orderedImports = imports.values.filter { s -> s.size == 1 }
            .map { s -> s.first() }
            .filter { s -> s.namespace != modName }
            .map { s -> "import ${s.namespace}" }
            .sorted()

        return orderedImports.joinToString(System.lineSeparator())
    }
}
