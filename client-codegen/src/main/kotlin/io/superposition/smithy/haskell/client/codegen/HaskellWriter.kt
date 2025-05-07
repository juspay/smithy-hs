package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolDependency
import software.amazon.smithy.codegen.core.SymbolWriter
import java.util.logging.Logger

@Suppress("MaxLineLength")
class HaskellWriter(val fileName: String, val modName: String) : SymbolWriter<HaskellWriter, HaskellImportContainer>(
    HaskellImportContainer(modName)
) {
    private val logger: Logger = Logger.getLogger(this.javaClass.name)

    init {
        setExpressionStart('#')
        putFormatter('D', this::dependencyFormatter)
        putFormatter('T', this::haskellTypeFormatter)
    }

    override fun toString(): String {
        val sb = StringBuilder()

        sb.appendLine("module $modName where")
        sb.appendLine()
        sb.appendLine(this.importContainer.toString())
        sb.appendLine()
        sb.appendLine(super.toString())

        return sb.toString()
    }

    private fun dependencyFormatter(type: Any, ignored: String): String {
        // Don't want to write dependencies outside the cabal file.
        require(fileName == CABAL_FILE)
        require(type is SymbolDependency)
        // TODO Handle rendering ranges.
        return "${type.packageName} ${type.version}"
    }

    private fun haskellTypeFormatter(sym: Any, indent: String): String {
        when (sym) {
            is Symbol -> {
                return renderSymbol(sym).joinToString(" ")
            }
            else -> error("$sym is not a Symbol.")
        }
    }

    private fun renderSymbol(sym: Symbol): List<String> {
        importContainer.importSymbol(sym, null)
        return when (sym.references.size) {
            0 -> listOf(sym.name)
            else -> {
                val refs = sym.references
                    .map {
                        var rendered = renderSymbol(it.symbol)
                        if (rendered.size > 1) {
                            return@map "(" + rendered.joinToString(" ") + ")"
                        } else {
                            return@map rendered.joinToString(" ")
                        }
                    }
                listOf(sym.name) + refs
            }
        }
    }

    class Factory(val settings: HaskellSettings) : SymbolWriter.Factory<HaskellWriter> {
        override fun apply(fileName: String, modName: String): HaskellWriter {
            return HaskellWriter(fileName, modName)
        }
    }

    companion object {
        const val CABAL_FILE = "project.cabal"
    }
}
