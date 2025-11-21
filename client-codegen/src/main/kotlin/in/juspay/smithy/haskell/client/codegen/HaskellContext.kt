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
    override fun model(): Model = model

    override fun settings(): HaskellSettings = settings

    override fun symbolProvider(): SymbolProvider = symbolProvider

    override fun fileManifest(): FileManifest = fileManifest

    override fun writerDelegator(): WriterDelegator<HaskellWriter> = writerDelegator

    override fun integrations(): List<HaskellIntegration> = integrations
}
