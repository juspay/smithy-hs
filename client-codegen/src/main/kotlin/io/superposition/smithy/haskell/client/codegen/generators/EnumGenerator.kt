@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Eq
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Generic
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.enumValue
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.Shape
import java.util.function.Consumer

@Suppress("MaxLineLength")
class EnumGenerator<T : ShapeDirective<Shape, HaskellContext, HaskellSettings>> :
    Consumer<T> {
    private val defaultDerives = listOf(Generic, Eq)

    override fun accept(directive: T) {
        val shape = directive.shape()
        val symbol = directive.symbol()

        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template = """
            -- Enum implementation for #{shape:T}
            data #{shape:T} =
                #{constructors:C|}
                #{derives:C|}

            #{serializer:C|}
            #{deserializer:C|}
            """.trimIndent()

            writer.pushState()
            writer.putContext("shape", directive.symbol())
            writer.putContext(
                "constructors",
                Runnable { generateConstructors(writer, shape) }
            )
            writer.putContext("derives", Runnable { generateDerives(writer) })
            writer.putContext(
                "serializer",
                Runnable { serializerGenerator(writer, shape, directive.symbol()) }
            )
            writer.putContext(
                "deserializer",
                Runnable { generateDeserializers(writer, shape, symbol) }
            )
            writer.write(template)
            writer.addExport("${symbol.name}(..)")
            writer.exposeModule()
            writer.popState()
        }
    }

    private fun generateConstructors(
        writer: HaskellWriter,
        shape: Shape,
    ) {
        for ((i, member) in shape.members().withIndex()) {
            if (i == 0) {
                writer.write(member.memberName)
            } else {
                writer.write("| ${member.memberName}")
            }
        }
    }

    private fun generateDerives(writer: HaskellWriter) {
        writer.openBlock("deriving (", ")") {
            writer.writeList(defaultDerives) { derive ->
                writer.format("#T", derive)
            }
        }
    }

    private fun generateDeserializers(
        writer: HaskellWriter,
        shape: Shape,
        symbol: Symbol,
    ) {
        val errMsg = "Unknown value for ${symbol.name}: ".dq
        writer.openBlock("instance #{aeson:N}.FromJSON ${symbol.name} where", "") {
            writer.openBlock(
                "parseJSON = #{aeson:N}.withText ${symbol.name.dq} $ \\v ->",
                ""
            ) {
                writer.openBlock("case v of", "") {
                    for (member in shape.members()) {
                        val constructor = member.memberName
                        writer.write(
                            "${member.enumValue.dq} -> pure $constructor"
                        )
                    }
                    writer.write("_ -> fail $ $errMsg <> #{text:N}.unpack v")
                }
            }
        }
    }

    private fun serializerGenerator(
        writer: HaskellWriter,
        shape: Shape,
        symbol: Symbol,
    ) {
        writer.openBlock("instance #{aeson:N}.ToJSON ${symbol.name} where", "") {
            for (member in shape.members()) {
                val enumValue = member.enumValue.dq
                writer.write(
                    "toJSON ${member.memberName} = #{aeson:N}.String $ #{text:N}.pack $enumValue"
                )
            }
        }
    }
}
