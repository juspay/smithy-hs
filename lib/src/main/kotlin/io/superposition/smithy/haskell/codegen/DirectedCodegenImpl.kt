import software.amazon.smithy.codegen.core.directed.DirectedCodegen

public class DirectedCodegenImpl :
        DirectedCodegen<HaskellContext, HaskellSettings, HaskellIntegration> {

    override fun createSymbolProvider(p0: CreateSymbolProviderDirective<(HaskellSettings..HaskellSettings?)>): SymbolProvider { }

    override fun createContext(p0: CreateContextDirective<(HaskellSettings..HaskellSettings?), (HaskellIntegration..HaskellIntegration?)>): HaskellContext { }

    override fun generateService(p0: GenerateServiceDirective<(HaskellContext..HaskellContext?), (HaskellSettings..HaskellSettings?)>) { }

    override fun generateStructure(p0: GenerateStructureDirective<(HaskellContext..HaskellContext?), (HaskellSettings..HaskellSettings?)>) { }

    override fun generateError(p0: GenerateErrorDirective<(HaskellContext..HaskellContext?), (HaskellSettings..HaskellSettings?)>) { }

    override fun generateUnion(p0: GenerateUnionDirective<(HaskellContext..HaskellContext?), (HaskellSettings..HaskellSettings?)>) { }

    override fun generateEnumShape(p0: GenerateEnumDirective<(HaskellContext..HaskellContext?), (HaskellSettings..HaskellSettings?)>) { }

    override fun generateIntEnumShape(p0: GenerateIntEnumDirective<(HaskellContext..HaskellContext?), (HaskellSettings..HaskellSettings?)>) { }}
