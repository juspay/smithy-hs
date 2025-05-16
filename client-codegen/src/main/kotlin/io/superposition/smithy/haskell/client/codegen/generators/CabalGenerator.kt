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
            writer.putContext("publicModules", Runnable { moduleWriter(writer, PUBLIC) })
            writer.putContext("privateModules", Runnable { moduleWriter(writer, PRIVATE) })
            writer.putContext(
                "hasPrivateModules",
                HaskellWriter.MODULES.filter { it.value == PRIVATE }.isNotEmpty()
            )
            writer.write(
                """
                cabal-version: 3.0
                name: ${ctx.settings.packageName}
                version: ${ctx.settings.version}
                category: Client
                build-type: Simple

                library
                    build-depends:    #{dependencies:C|}
                    exposed-modules:  #{publicModules:C|}
                #{?hasPrivateModules}
                    other-modules:    #{privateModules:C|}
                #{/hasPrivateModules}
                    default-language: Haskell2010
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
            .distinctBy { it.packageName }
        val itr = dependencies.iterator()
        while (itr.hasNext()) {
            val d = itr.next()
            writer.writeInline("#D", d)
            if (itr.hasNext()) {
                writer.writeInline(",")
            }
            writer.write("")
        }
    }

    private fun moduleWriter(writer: HaskellWriter, type: Boolean) {
        val itr = HaskellWriter.MODULES.filter { it.value == type }.iterator()
        while (itr.hasNext()) {
            writer.writeInline(itr.next().key)
            if (itr.hasNext()) {
                writer.writeInline(",")
            }
            writer.write("")
        }
    }

    companion object {
        const val PUBLIC = true
        const val PRIVATE = false
    }
}
