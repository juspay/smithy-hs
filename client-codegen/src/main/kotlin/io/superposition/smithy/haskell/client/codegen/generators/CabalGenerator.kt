package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.CodegenUtils
import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import software.amazon.smithy.codegen.core.SymbolDependency
import software.amazon.smithy.codegen.core.directed.CustomizeDirective

class CabalGenerator(directive: CustomizeDirective<HaskellContext, HaskellSettings>) : Runnable {
    private val ctx = directive.context()
    override fun run() {
        ctx.writerDelegator().useFileWriter(HaskellWriter.CABAL_FILE) { writer ->
            writer.putContext("dependencies", Runnable { dependencyWriter(writer) })
            // REVIEW Maybe we can use a list formatter for such cases.
            writer.putContext("publicModules", Runnable { moduleWriter(writer, PUBLIC) })
            if (HaskellWriter.MODULES.filter { it.value == PRIVATE }.isNotEmpty()) {
                writer.putContext("privateModules", Runnable { moduleWriter(writer, PRIVATE) })
            }
            writer.write(
                """
                cabal-version: 3.0
                name: ${ctx.settings.packageName}
                version: ${ctx.settings.version}
                category: Client
                build-type: Simple

                library
                    build-depends:      #{dependencies:C|}
                    exposed-modules:    #{publicModules:C|}
                #{?privateModules}
                    other-modules:      #{privateModules:C|}
                #{/privateModules}
                    default-language:   Haskell2010
                    default-extensions: OverloadedStrings
                """.trimIndent()
            )
        }
    }

    private fun dependencyWriter(writer: HaskellWriter) {
        val base = SymbolDependency.builder()
            .packageName("base")
            .version(CodegenUtils.depRange("4.14", "4.19"))
            .build()
        val dependencies = (listOf(base) + ctx.writerDelegator().dependencies)
            .distinctBy { Pair(it.packageName, it.version) }
        writer.writeList(dependencies) { writer.format("#D", it) }
    }

    private fun moduleWriter(writer: HaskellWriter, type: Boolean) {
        val ms = HaskellWriter.MODULES.filter { it.value == type }.toList()
        writer.writeList(ms) { it.first }
    }

    companion object {
        const val PUBLIC = true
        const val PRIVATE = false
    }
}
