package `in`.juspay.smithy.haskell.client.codegen

import software.amazon.smithy.build.FileManifest
import software.amazon.smithy.codegen.core.CodegenContext
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.WriterDelegator
import software.amazon.smithy.model.Model

public data class HaskellContext(
    val model: Model,
    val settings: HaskellSettings,
    val symbolProvider: SymbolProvider,
    val fileManifest: FileManifest,
    val writerDelegator: WriterDelegator<HaskellWriter>,
    val integrations: List<HaskellIntegration>,
    val utilitySymbol: Symbol,
) : CodegenContext<HaskellSettings, HaskellWriter, HaskellIntegration> {

    override fun model(): Model {
        return model
    }

    override fun settings(): HaskellSettings {
        return settings
    }

    override fun symbolProvider(): SymbolProvider {
        return symbolProvider
    }

    override fun fileManifest(): FileManifest {
        return fileManifest
    }

    override fun writerDelegator(): WriterDelegator<HaskellWriter> {
        return writerDelegator
    }

    override fun integrations(): List<HaskellIntegration> {
        return integrations
    }
}
