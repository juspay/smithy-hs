@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.jsonName
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.UnionShape
import software.amazon.smithy.utils.CaseUtils
import java.util.function.Consumer

@Suppress("MaxLineLength")
class UnionGenerator<T : ShapeDirective<UnionShape, HaskellContext, HaskellSettings>> :
    Consumer<T> {
    companion object {
        private fun getConstructorName(
            member: MemberShape
        ): String {
            return CaseUtils.toPascalCase(member.memberName)
        }
    }

    override fun accept(directive: T) {
        val union = directive.shape()

        directive.context().writerDelegator().useShapeWriter(union) { writer ->
            val template = """
            -- Union implementation for #{shape:T}
            data #{shape:T} =
                #{constructors:C|}

            #{serializer:C|}
            #{deserializer:C|}
            """.trimIndent()

            writer.pushState()
            writer.putContext("shape", directive.symbol())
            writer.putContext(
                "constructors",
                Runnable {
                    generateConstructor(
                        writer,
                        directive.symbolProvider(),
                        union
                    )
                }
            )
            writer.putContext(
                "serializer",
                Runnable { generateSerializer(writer, union, directive.symbol()) }
            )
            writer.putContext(
                "deserializer",
                Runnable { generateDeserializer(writer, union, directive.symbol()) }
            )
            writer.write(template)
            writer.popState()
            writer.exposeModule()
            writer.addExport(directive.symbol().name)
        }
    }

    private fun generateConstructor(
        writer: HaskellWriter,
        symbolProvider: SymbolProvider,
        shape: UnionShape
    ) {
        for ((i, member) in shape.members().withIndex()) {
            val memberSymbol = symbolProvider.toSymbol(member)
            val constructor = getConstructorName(member)
            if (i != 0) {
                writer.write("| ")
            }
            writer.write("$constructor (#T)", memberSymbol)
        }
    }

    private fun generateSerializer(
        writer: HaskellWriter,
        shape: Shape,
        symbol: Symbol,
    ) {
        writer.openBlock("instance #{aeson:N}.ToJSON ${symbol.name} where", "") {
            for (member in shape.members()) {
                val jsonName = member.jsonName
                val constructor = getConstructorName(member)
                writer.write("toJSON ($constructor a) = #{aeson:N}.object [ ${jsonName.dq} #{aeson:N}..= a ]")
            }
        }
    }

    private fun generateDeserializer(
        writer: HaskellWriter,
        shape: Shape,
        symbol: Symbol,
    ) {
        val errMsg =
            "Could not parse ${symbol.name}. Expected an object with one of keys: ${
                shape.members().joinToString { it.memberName }
            }.".dq
        val structName = symbol.name
        writer.openBlock("instance #{aeson:N}.FromJSON $structName where", "") {
            writer.openBlock(
                "parseJSON = #{aeson:N}.withObject ${structName.dq} $ \\v ->",
                ""
            ) {
                for (member in shape.members()) {
                    val constructor = getConstructorName(member)
                    val jsonName = member.jsonName.dq
                    writer.write(
                        "($constructor #{functor:N}.<$> v #{aeson:N}..: $jsonName) #{alternative:N}.<|>"
                    )
                }
                writer.write("fail $errMsg")
            }
        }
    }
}
