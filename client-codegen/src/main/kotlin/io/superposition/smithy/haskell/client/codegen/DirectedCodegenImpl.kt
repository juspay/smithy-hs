package io.superposition.smithy.haskell.client.codegen

import io.superposition.smithy.haskell.client.codegen.generators.*
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.WriterDelegator
import software.amazon.smithy.codegen.core.directed.*

class DirectedCodegenImpl :
    DirectedCodegen<HaskellContext, HaskellSettings, HaskellIntegration> {

    override fun createSymbolProvider(
        directive: CreateSymbolProviderDirective<HaskellSettings>
    ): SymbolProvider {
        return HaskellSymbolProvider(
            directive.model(),
            directive.service(),
            directive.service().id.namespace,
        )
    }

    override fun createContext(
        directive: CreateContextDirective<HaskellSettings, HaskellIntegration>
    ): HaskellContext {
        val sp = directive.symbolProvider()
        val writerDelegator =
            WriterDelegator(
                directive.fileManifest(),
                sp,
                HaskellWriter.Factory(directive.settings())
            )

        return HaskellContext(
            model = directive.model(),
            settings = directive.settings(),
            symbolProvider = sp,
            fileManifest = directive.fileManifest(),
            writerDeligator = writerDelegator,
            integrations = directive.integrations()
        )
    }

    override fun generateService(
        directive: GenerateServiceDirective<HaskellContext, HaskellSettings>
    ) {
        ServiceGenerator<GenerateServiceDirective<HaskellContext, HaskellSettings>>().accept(directive)
    }

    override fun generateStructure(
        directive: GenerateStructureDirective<HaskellContext, HaskellSettings>
    ) {
        StructureGenerator<GenerateStructureDirective<HaskellContext, HaskellSettings>>().accept(directive)
    }

    override fun generateOperation(directive: GenerateOperationDirective<HaskellContext, HaskellSettings>) {
        OperationGenerator<GenerateOperationDirective<HaskellContext, HaskellSettings>>().accept(directive)
    }

    override fun generateEnumShape(
        directive: GenerateEnumDirective<HaskellContext, HaskellSettings>
    ) {
        EnumGenerator<GenerateEnumDirective<HaskellContext, HaskellSettings>>().accept(directive)
    }

    override fun generateError(directive: GenerateErrorDirective<HaskellContext, HaskellSettings>) {
        ErrorGenerator<GenerateErrorDirective<HaskellContext, HaskellSettings>>().accept(directive)
    }

    override fun generateUnion(directive: GenerateUnionDirective<HaskellContext, HaskellSettings>) {
        UnionGenerator<GenerateUnionDirective<HaskellContext, HaskellSettings>>().accept(directive)
    }

    override fun generateIntEnumShape(
        directive: GenerateIntEnumDirective<HaskellContext, HaskellSettings>
    ) {
        IntEnumGenerator<GenerateIntEnumDirective<HaskellContext, HaskellSettings>>().accept(directive)
    }

    // This where we are supposed to generate things like dependency manifests and READMEs.
    override fun customizeAfterIntegrations(directive: CustomizeDirective<HaskellContext, HaskellSettings>?) {
        super.customizeAfterIntegrations(directive)
    }
}
