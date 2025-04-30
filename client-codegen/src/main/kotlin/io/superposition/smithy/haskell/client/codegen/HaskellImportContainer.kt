package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.ImportContainer
import software.amazon.smithy.codegen.core.Symbol
import java.util.TreeSet

class HaskellImportContainer(val modName: String) : ImportContainer {
    private data class ImportEntry(
        val symbol: Symbol,
        val alias: String?
    )

    private val imports: MutableMap<String, MutableSet<ImportEntry>> = HashMap();
    override fun importSymbol(symbol: Symbol, alias: String?) {
        if (symbol.isPrimitive()) {
            return
        }
        val duplicates = imports.computeIfAbsent(symbol.name) { HashSet() }
        duplicates.add(ImportEntry(symbol, alias))
    }

    override fun toString(): String {
        val builder = StringBuilder();
        val validImports = imports.values.filter { s -> s.size == 1 }
            .map { s -> s.first() }
            .filter { s -> s.symbol.namespace != modName }
            .toCollection(TreeSet());

        for (entry in validImports) {
            builder.append("import ${entry.symbol.namespace}");
            if (entry.alias != null) {
                builder.append(" as ${entry.alias}");
            }
            builder.append(System.lineSeparator());
        }

        return builder.toString();
    }
}
