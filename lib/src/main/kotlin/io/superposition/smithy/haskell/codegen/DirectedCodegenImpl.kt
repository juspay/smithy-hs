package io.superposition.smithy.haskell.codegen

import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.directed.CreateContextDirective
import software.amazon.smithy.codegen.core.directed.CreateSymbolProviderDirective
import software.amazon.smithy.codegen.core.directed.DirectedCodegen
import software.amazon.smithy.codegen.core.directed.GenerateEnumDirective
import software.amazon.smithy.codegen.core.directed.GenerateErrorDirective
import software.amazon.smithy.codegen.core.directed.GenerateIntEnumDirective
import software.amazon.smithy.codegen.core.directed.GenerateServiceDirective
import software.amazon.smithy.codegen.core.directed.GenerateStructureDirective
import software.amazon.smithy.codegen.core.directed.GenerateUnionDirective

public class DirectedCodegenImpl :
        DirectedCodegen<HaskellContext, HaskellSettings, HaskellIntegration> {

    override fun createSymbolProvider(
            directive: CreateSymbolProviderDirective<HaskellSettings>
    ): SymbolProvider {
        return HaskellSymbolProvider()
    }

    override fun createContext(
            directive: CreateContextDirective<HaskellSettings, HaskellIntegration>
    ): HaskellContext {
        return HaskellContext(
                directive.model(),
                directive.settings(),
                directive.symbolProvider(),
                directive.fileManifest(),
                directive.integrations(),
        )
    }

    override fun generateService(
            directive: GenerateServiceDirective<HaskellContext, HaskellSettings>
    ) {
        TODO("Not yet implemented")
    }

    override fun generateStructure(
            directive: GenerateStructureDirective<HaskellContext, HaskellSettings>
    ) {
        TODO("Not yet implemented")
    }

    override fun generateError(directive: GenerateErrorDirective<HaskellContext, HaskellSettings>) {
        TODO("Not yet implemented")
    }

    override fun generateUnion(directive: GenerateUnionDirective<HaskellContext, HaskellSettings>) {
        TODO("Not yet implemented")
    }

    override fun generateEnumShape(
            directive: GenerateEnumDirective<HaskellContext, HaskellSettings>
    ) {
        TODO("Not yet implemented")
    }

    override fun generateIntEnumShape(
            directive: GenerateIntEnumDirective<HaskellContext, HaskellSettings>
    ) {
        TODO("Not yet implemented")
    }
}
