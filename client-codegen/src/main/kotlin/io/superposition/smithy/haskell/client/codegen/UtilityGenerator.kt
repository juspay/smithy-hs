@file:Suppress("MaxLineLength")

package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.directed.CustomizeDirective

class UtilityGenerator(
    private val directive: CustomizeDirective<HaskellContext, HaskellSettings>
) : Runnable {
    private final val UTILITY_FILE =
        this.javaClass.getResourceAsStream("/utility.hs")

    override fun run() {
        directive.context().writerDelegator()
            .useSymbolWriter(directive.context().utilitySymbol) { writer ->
                val content = UTILITY_FILE?.bufferedReader()?.readText()
                    ?: throw IllegalStateException("Utility file not found.")

                writer.addExport("RequestSegment")
                writer.addExport("toRequestSegment")
                writer.write(content)
            }
    }
}
