package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.*
import `in`.juspay.smithy.haskell.client.codegen.Http.MTLState
import `in`.juspay.smithy.haskell.client.codegen.language.Record
import software.amazon.smithy.codegen.core.Symbol

class BuilderGenerator(
    val record: Record,
    val symbol: Symbol,
    val writer: HaskellWriter,
    val skipFields: List<String>? = null,
    val extraSetters: List<String>? = null,
) : Runnable {
    private data class BuilderStateMember(
        val name: String,
        val symbol: Symbol,
        val inputName: String,
        val inputSymbol: Symbol,
    )

    private val builderName = "${record.name}Builder"
    private val stateName = "${builderName}State"
    private val builderStateMembers =
        record.fields.map {
            val name = "${it.name}BuilderState"
            val symbol = it.symbol.toMaybe()
            return@map BuilderStateMember(name, symbol, it.name, it.symbol)
        }

    override fun run() {
        writer.pushState()
        writer.putContext("builderName", builderName)
        writer.putContext("builderState", Runnable(::builderStateSection))
        writer.putContext("defaultBuilderState", Runnable(::defaultBuilderState))
        writer.putContext("builderSetters", Runnable(::builderSetters))
        writer.putContext("builderFunction", Runnable(::builderFunction))
        writer.putContext("mtlState", MTLState)
        writer.putContext("mtlRunState", MTLState.toBuilder().name("runState").build())
        writer.putContext("mtlModify", MTLState.toBuilder().name("modify").build())
        writer.write(
            """

            #{builderState:C}

            #{defaultBuilderState:C}

            type $builderName = #{mtlState:T} $stateName
            #{builderSetters:C}

            #{builderFunction:C}
            """.trimIndent(),
        )
        writer.addExport(builderName)
        writer.popState()
    }

    private fun builderStateSection() {
        val record =
            Record(
                stateName,
                builderStateMembers.map {
                    Record.Field(
                        it.name,
                        it.symbol,
                    )
                },
            )
        writer.writeRecord(record)
    }

    private fun defaultBuilderState() {
        val fn = "defaultBuilderState"
        writer.write("$fn :: $stateName")
        writer.openBlock("$fn = $stateName {", "}") {
            writer.writeList(builderStateMembers) {
                writer.format("${it.name} = #{nothing:T}")
            }
        }
    }

    @Suppress("MaxLineLength")
    private fun builderSetters() {
        builderStateMembers.forEach {
            if (skipFields?.contains(it.inputName) ?: false) {
                return@forEach
            }
            val fn = CodegenUtils.getSetterName(it.inputName)
            writer.addExport(fn)
            writer.putContext("isMaybe", it.inputSymbol.isMaybe())
            writer.write(
                """

                $fn :: #T -> $builderName ()
                $fn value =
                   #{mtlModify:T} (\s -> (s { ${it.name} = #{^isMaybe}#{just:T} #{/isMaybe}value }))
                """.trimIndent(),
                it.inputSymbol,
            )
        }
        extraSetters?.forEach { writer.write(it) }
    }

    private fun builderFunction() {
        val fn = "build"
        writer.addExport(fn)
        writer.write("$fn :: $builderName () -> #{either:T} #{text:T} ${record.name}")
        writer.openBlock("$fn builder = do", "") {
            writer.write("let (_, st) = #{mtlRunState:T} builder defaultBuilderState")
            builderStateMembers.forEach {
                val mn = it.inputName
                val e = "\"$symbol.$mn is a required property.\""
                if (it.inputSymbol.isMaybe()) {
                    writer.write("$mn' <- #{right:T} (${it.name} st)")
                } else {
                    writer.write(
                        "$mn' <- Data.Maybe.maybe (#{left:T} $e) #{right:T} (${it.name} st)",
                    )
                }
            }
            writer.openBlock("#{right:T} (#T { ", "})", symbol) {
                writer.writeList(builderStateMembers) {
                    "${it.inputName} = ${it.inputName}'"
                }
            }
        }
    }
}
