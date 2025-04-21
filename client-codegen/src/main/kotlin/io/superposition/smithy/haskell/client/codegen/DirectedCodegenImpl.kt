package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.directed.DirectedCodegen

public class DirectedCodegenImpl :
        DirectedCodegen<HaskellContext, HaskellSettings, HaskellIntegration> {

        override fun createSymbolProvider(
                directive: CreateSymbolProviderDirective<HaskellSettings>
        ): SymbolProvider {
            return HaskellSymbolProvider(directive.model(), 
                directive.service(),
                directive.settings().packageNamespace)
)
        }

        override fun createContext(
                directive: CreateContextDirective<HaskellSettings, HaskellIntegration>
        ): HaskellContext {}

        override fun generateService(
                directive: GenerateServiceDirective<HaskellContext, HaskellSettings>
        ) {}

        override fun generateStructure(
                directive: GenerateStructureDirective<HaskellContext, HaskellSettings>
        ) {}

        override fun generateError(
                directive: GenerateErrorDirective<HaskellContext, HaskellSettings>
        ) {}

        override fun generateUnion(
                directive: GenerateUnionDirective<HaskellContext, HaskellSettings>
        ) {}

        override fun generateEnumShape(
                directive: GenerateEnumDirective<HaskellContext, HaskellSettings>
        ) {}

        override fun generateIntEnumShape(
                directive: GenerateIntEnumDirective<HaskellContext, HaskellSettings>
        ) {}
}
