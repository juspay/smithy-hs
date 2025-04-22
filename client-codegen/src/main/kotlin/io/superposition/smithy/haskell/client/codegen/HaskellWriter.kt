package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.SymbolWriter

public class HaskellWriter(val modName: String) :
        SymbolWriter<HaskellWriter, ImportContainerImpl>(ImportContainerImpl(modName)) {

    public class Factory(val settings: HaskellSettings) : SymbolWriter.Factory<HaskellWriter> {
        override fun apply(fileName: String, modName: String): HaskellWriter {
            return HaskellWriter(modName)
        }
    }
}
