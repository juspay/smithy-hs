package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolDependency
import software.amazon.smithy.codegen.core.SymbolWriter
import java.util.ArrayList
import java.util.concurrent.ConcurrentHashMap
import java.util.logging.Logger

class HaskellWriter(
    val fileName: String,
    val modName: String
) : SymbolWriter<HaskellWriter, HaskellImportContainer>(
    HaskellImportContainer(modName)
) {
    private val logger: Logger = Logger.getLogger(this.javaClass.name)
    private val exports: MutableList<String> = ArrayList()
    private val isSourceFile = fileName.endsWith(".hs")

    init {
        setExpressionStart('#')
        putFormatter('D', this::dependencyFormatter)
        putFormatter('T', this::haskellTypeFormatter)
        putDefaultContext()
        if (isSourceFile) {
            MODULES.set(modName, false)
        }
    }

    override fun toString(): String {
        if (!isSourceFile) {
            return super.toString()
        }

        val sb = StringBuilder()

        sb.appendLine("module $modName (")
        exports.forEachIndexed { i, e ->
            sb.append("    ")
            if (i > 0) {
                sb.append(", ")
            } else {
                sb.append("  ")
            }
            sb.appendLine(e)
        }
        sb.appendLine(") where")
        sb.appendLine()
        sb.appendLine(this.importContainer.toString())
        sb.appendLine()
        sb.appendLine(super.toString())

        return sb.toString()
    }

    fun exposeModule() {
        MODULES.set(modName, true)
    }

    fun addExport(export: String) {
        exports.add(export)
    }

    override fun pushState(): HaskellWriter {
        super.pushState()
        putDefaultContext()
        return this
    }

    private fun putDefaultContext() {
        putContext("functor", HaskellSymbol.Functor)
        putContext("applicative", HaskellSymbol.Applicative)
        putContext("monad", HaskellSymbol.Monad)
        putContext("either", HaskellSymbol.Either)
        putContext("maybe", HaskellSymbol.Maybe)
        putContext("text", HaskellSymbol.Text)
        putContext("just", HaskellSymbol.Maybe.toBuilder().name("Just").build())
        putContext("nothing", HaskellSymbol.Maybe.toBuilder().name("Nothing").build())
        putContext("right", HaskellSymbol.Either.toBuilder().name("Right").build())
        putContext("left", HaskellSymbol.Either.toBuilder().name("Left").build())
    }

    private fun dependencyFormatter(type: Any, ignored: String): String {
        // Don't want to write dependencies outside the cabal file.
        require(fileName == CABAL_FILE)
        require(type is SymbolDependency)
        // TODO Handle rendering ranges.
        return "${type.packageName} ${type.version}"
    }

    private fun haskellTypeFormatter(sym: Any, indent: String): String {
        when (sym) {
            is Symbol -> {
                return renderSymbol(sym).joinToString(" ")
            }
            else -> error("$sym is not a Symbol.")
        }
    }

    private fun renderSymbol(sym: Symbol): List<String> {
        addDependency(sym)
        importContainer.importSymbol(sym, null)
        return when (sym.references.size) {
            0 -> listOf(sym.relativize(modName))
            else -> {
                val refs = sym.references
                    .map {
                        var rendered = renderSymbol(it.symbol)
                        if (rendered.size > 1) {
                            return@map "(" + rendered.joinToString(" ") + ")"
                        } else {
                            return@map rendered.joinToString(" ")
                        }
                    }
                listOf(sym.relativize(modName)) + refs
            }
        }
    }

    class Factory(val settings: HaskellSettings) : SymbolWriter.Factory<HaskellWriter> {
        override fun apply(fileName: String, modName: String): HaskellWriter {
            return HaskellWriter(fileName, modName)
        }
    }

    companion object {
        const val CABAL_FILE = "project.cabal"
        val MODULES: MutableMap<String, Boolean> = ConcurrentHashMap()
    }
}
