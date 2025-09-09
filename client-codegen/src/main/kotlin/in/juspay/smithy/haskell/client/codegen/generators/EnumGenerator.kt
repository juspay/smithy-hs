@file:Suppress(
    "FINITE_BOUNDS_VIOLATION_IN_JAVA",
    "ktlint:standard:max-line-length",
)

package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.*
import `in`.juspay.smithy.haskell.client.codegen.CodegenUtils.dq
import `in`.juspay.smithy.haskell.client.codegen.HaskellSymbol.Eq
import `in`.juspay.smithy.haskell.client.codegen.HaskellSymbol.Generic
import `in`.juspay.smithy.haskell.client.codegen.HaskellSymbol.Show
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.Shape
import java.util.function.Consumer

class EnumGenerator<T : ShapeDirective<Shape, HaskellContext, HaskellSettings>> :
    Consumer<T> {
    private val defaultDerives = listOf(Generic, Eq, Show)

    override fun accept(directive: T) {
        val shape = directive.shape()
        val symbol = directive.symbol()

        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template =
                """
                -- Enum implementation for #{shape:T}
                data #{shape:T} =
                    #{constructors:C|}
                    #{derives:C|}

                #{serializer:C|}
                #{deserializer:C|}
                #{serDe:C}
                """.trimIndent()

            writer.pushState()
            writer.putContext("shape", directive.symbol())
            writer.putContext("utility", directive.context().utilitySymbol)
            writer.putContext("encoding", HaskellSymbol.EncodingUtf8)
            writer.putContext(
                "constructors",
                Runnable { generateConstructors(writer, shape) },
            )
            writer.putContext("derives", Runnable { generateDerives(writer) })
            writer.putContext(
                "serializer",
                Runnable { serializerGenerator(writer, shape, directive.symbol()) },
            )
            writer.putContext(
                "deserializer",
                Runnable { generateDeserializers(writer, shape, symbol) },
            )
            writer.putContext("serDe", Runnable { serDeGenerator(writer, shape, symbol) })
            writer.write(template)
            writer.addExport("${symbol.name}(..)")
            writer.exposeModule()
            writer.popState()
        }
    }

    private fun generateConstructors(writer: HaskellWriter, shape: Shape) {
        for ((i, member) in shape.members().withIndex()) {
            if (i == 0) {
                writer.write(member.fieldName)
            } else {
                writer.write("| ${member.fieldName}")
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
                "",
            ) {
                writer.openBlock("case v of", "") {
                    for (member in shape.members()) {
                        val constructor = member.fieldName
                        writer.write(
                            "${member.enumValue.dq} -> pure $constructor",
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
                    "toJSON ${member.fieldName} = #{aeson:N}.String $ #{text:N}.pack $enumValue",
                )
            }
        }
    }

    private fun serDeGenerator(writer: HaskellWriter, shape: Shape, symbol: Symbol) {
        writer.openBlock("instance #{utility:N}.SerDe ${symbol.name} where", "") {
            for (member in shape.members()) {
                val value = member.enumValue.dq
                writer.write(
                    "serializeElement ${member.fieldName} = #{encoding:N}.encodeUtf8 $ #{text:N}.pack $value",
                )
            }
            writer.openBlock(
                "deSerializeElement bs = case #{encoding:N}.decodeUtf8 bs of",
                "",
            ) {
                for (member in shape.members()) {
                    val value = member.enumValue.dq
                    writer.write("$value -> Right ${member.fieldName}")
                }
                writer.write(
                    "e -> Left (${"Failed to de-serialize ${symbol.name}, encountered unknown variant: ".dq} ++ (show bs))",
                )
            }
        }
    }
}
