package io.superposition.smithy.haskell.client.codegen

import io.superposition.smithy.haskell.client.codegen.language.Record
import software.amazon.smithy.codegen.core.CodegenException
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolDependency
import software.amazon.smithy.codegen.core.SymbolWriter
import java.util.ArrayList
import java.util.concurrent.ConcurrentHashMap
import java.util.logging.Logger

@Suppress("TooManyFunctions")
class HaskellWriter(
    val fileName: String,
    val modName: String
) : SymbolWriter<HaskellWriter, HaskellImportContainer>(
    HaskellImportContainer(modName)
) {
    private val logger: Logger = Logger.getLogger(this.javaClass.name)
    private val exports: MutableList<String> = ArrayList()
    private val isSourceFile = fileName.endsWith(".hs")
    private val languageExts: List<String> = listOf(
        "DeriveGeneric",
        // "DeriveAnyClass",
        "OverloadedStrings",
        // "DuplicateRecordFields",
        // "RecordWildCards",
        // "NamedFieldPuns",
        // "TypeApplications",
        // "FlexibleContexts",
        // "MultiParamTypeClasses",
        // "FunctionalDependencies",
        // "TypeFamilies",
        // "GADTs",
        // "GeneralizedNewtypeDeriving",
    )

    init {
        setExpressionStart('#')
        putFormatter('D', this::dependencyFormatter)
        putFormatter('T', this::haskellTypeFormatter)
        putFormatter('N', this::namespaceFormatter)
        putDefaultContext()
        if (isSourceFile) {
            if (modName.isEmpty()) throw CodegenException("Module name is empty.")
            MODULES.set(modName, false)
        }
    }

    override fun toString(): String {
        if (!isSourceFile) {
            return super.toString()
        }

        val sb = StringBuilder()

        for (langExt in languageExts) {
            sb.appendLine("{-# LANGUAGE $langExt #-}")
        }
        sb.appendLine()

        sb.appendLine("module $modName (")
        sb.appendLine(exports.map { "    " + it }.joinToString(",\n"))
        sb.appendLine(") where")

        sb.appendLine(this.importContainer.toString())
        sb.appendLine()
        // require(getContext("QueryString") is Symbol)
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
        putContext("manager", HaskellSymbol.Http.Manager)
        putContext("list", HaskellSymbol.List)
        putContext("map", HaskellSymbol.Map)
        putContext("aeson", HaskellSymbol.Aeson)
        putContext("byteString", HaskellSymbol.ByteString)
        putContext("flip", HaskellSymbol.Flip)
        putContext("query", Http.Query)
        putContext("httpClient", Http.HttpClient)
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

    private fun namespaceFormatter(sym: Any, ignored: String): String {
        require(sym is Symbol)
        importContainer.importSymbol(sym, null)
        addDependency(sym)
        return sym.namespace
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

    fun <T>writeList(list: List<T>, toLine: (T) -> String) {
        list.forEachIndexed { i, l ->
            writeInlineWithNoFormatting(toLine(l))
            if (i < list.size - 1) {
                write(",")
            } else {
                write("")
            }
        }
    }

    private fun writeDerives(derives: List<Symbol>) {
        write("deriving (")
        writeList(derives) { super.format("#T", it) }
        write(")")
    }

    fun writeRecord(record: Record) {
        putContext("derives", Runnable { writeDerives(record.defaultDerives) })
        openBlock("data ${record.name} = ${record.name} {", "} #{derives:C|}") {
            writeList(record.fields) { super.format("${it.name} :: #T", it.symbol) }
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
