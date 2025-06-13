package `in`.juspay.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.CodegenException
import software.amazon.smithy.codegen.core.ImportContainer
import software.amazon.smithy.codegen.core.Symbol
import java.util.TreeSet
import java.util.logging.Logger

class HaskellImportContainer(private val modName: String) : ImportContainer {
    private val imports: MutableMap<String, MutableSet<Symbol>> = HashMap()
    private val logger: Logger = Logger.getLogger(this.javaClass.name)

    companion object {
        private val expectionList = listOf(
            "()",
            "Integer",
            "Bool",
            "IO",
            "[]"
        )
    }

    override fun importSymbol(symbol: Symbol, alias: String?) {
        if (expectionList.contains(symbol.name) && symbol.namespace.isEmpty()) return

        if (symbol.namespace.isEmpty()) {
            throw CodegenException("Symbol ${symbol.name} namespace is empty.")
        }
        if (symbol.isPrimitive()) {
            return
        }
        val duplicates = imports.computeIfAbsent(symbol.name) { HashSet() }
        if (duplicates.isEmpty() || duplicates.first().namespace != symbol.namespace) {
            duplicates.add(symbol)
        }
    }

    override fun toString(): String {
        val orderedImports = imports.values
            .flatten()
            .filter { s -> s.namespace != modName }
            .map { s -> "import qualified ${s.namespace}" }
            .toCollection(TreeSet())

        return orderedImports.joinToString(System.lineSeparator())
    }
}
