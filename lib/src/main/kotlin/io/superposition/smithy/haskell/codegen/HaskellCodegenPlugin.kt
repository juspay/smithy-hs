package io.superposition.smithy.haskell.codegen

import software.amazon.smithy.build.PluginContext
import software.amazon.smithy.build.SmithyBuildPlugin

public class HaskellCodegenPlugin : SmithyBuildPlugin {
    // Tell Smithy-Build which plugin this is.
    override fun getName(): String {
        return "haskell-client-codegen"
    }

    override fun execute(pluginContext: PluginContext) {}
}
