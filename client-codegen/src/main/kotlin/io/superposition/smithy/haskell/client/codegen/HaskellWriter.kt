package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.SymbolDependency
import software.amazon.smithy.codegen.core.SymbolWriter

public class HaskellWriter(val fileName: String, val modName: String) :
    SymbolWriter<HaskellWriter, HaskellImportContainer>(HaskellImportContainer(modName)) {

    init {
        super.setRelativizeSymbols(modName)
        setExpressionStart('#')
        putFormatter('D', this::dependencyFormatter)
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

    class Factory(val settings: HaskellSettings) : SymbolWriter.Factory<HaskellWriter> {
        override fun apply(fileName: String, modName: String): HaskellWriter {
            return HaskellWriter(fileName, modName)
        }
    }

    companion object {
        const val CABAL_FILE = "project.cabal"
    }
}
