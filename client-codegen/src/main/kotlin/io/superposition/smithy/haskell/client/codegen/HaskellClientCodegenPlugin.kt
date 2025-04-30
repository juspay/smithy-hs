package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.build.PluginContext
import software.amazon.smithy.build.SmithyBuildPlugin
import software.amazon.smithy.codegen.core.directed.CodegenDirector
import software.amazon.smithy.codegen.core.directed.DirectedCodegen

class HaskellClientCodegenPlugin : SmithyBuildPlugin {
    override fun getName(): String = "haskell-client-codegen"

    override fun execute(pluginContext: PluginContext) {
        // Get the model from the plugin context
        val model = pluginContext.model

        // Create the directed codegen implementation
        val directedCodegen: DirectedCodegen<HaskellContext, HaskellSettings, HaskellIntegration> =
            DirectedCodegenImpl()

        // Use CodegenDirector to execute the code generation
        val codegenDirector: CodegenDirector<HaskellWriter, HaskellIntegration, HaskellContext, HaskellSettings> =
            CodegenDirector()
        val settings = HaskellSettings.fromNode(pluginContext.settings)
        codegenDirector.model(model)
        codegenDirector.fileManifest(pluginContext.fileManifest)
        codegenDirector.settings(settings)
        codegenDirector.service(settings.service)
        codegenDirector.integrationClass(HaskellIntegration::class.java)
        // REVIEW What does this mean?
        codegenDirector.changeStringEnumsToEnumShapes(true)
        codegenDirector.flattenPaginationInfoIntoOperations()
        codegenDirector.performDefaultCodegenTransforms()
        codegenDirector.createDedicatedInputsAndOutputs()
        codegenDirector.directedCodegen(directedCodegen)
        codegenDirector.run()
    }
}
