package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.SymbolWriter

public class HaskellWriter(val modName: String) :
    SymbolWriter<HaskellWriter, HaskellImportContainer>(HaskellImportContainer(modName)) {

    init {
        super.setRelativizeSymbols(modName)
        setExpressionStart('#')
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

    public class Factory(val settings: HaskellSettings) : SymbolWriter.Factory<HaskellWriter> {
        override fun apply(fileName: String, modName: String): HaskellWriter {
            return HaskellWriter(modName)
        }
    }
}
