@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.JsonObjectBuilder
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.ToJSON
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.UnionShape
import software.amazon.smithy.model.traits.JsonNameTrait
import software.amazon.smithy.utils.CaseUtils
import java.util.function.Consumer

@Suppress("MaxLineLength")
class UnionGenerator<T : ShapeDirective<UnionShape, HaskellContext, HaskellSettings>> : Consumer<T> {
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
            """.trimIndent()

            writer.pushState()
            writer.putContext("shape", directive.symbol())
            writer.putContext("constructors", ConstructorGenerator(writer, directive.symbolProvider(), union))
            writer.putContext("serializer", SerializerGenerator(writer, union, directive.symbol()))
            writer.write(template)
            writer.popState()
        }
    }

    private class ConstructorGenerator(
        private val writer: HaskellWriter,
        private val symbolProvider: SymbolProvider,
        private val shape: UnionShape,
    ) : Runnable {
        override fun run() {
            for ((i, member) in shape.members().withIndex()) {
                writer.pushState()
                writer.putContext(
                    "constructor",
                    getConstructorName(member)
                )
                writer.putContext("type", symbolProvider.toSymbol(member))
                if (i == 0) {
                    writer.write("#{constructor:L} #{type:T}")
                } else {
                    writer.write("| #{constructor:L} #{type:T}")
                }
                writer.popState()
            }
        }
    }

    private class SerializerGenerator(
        private val writer: HaskellWriter,
        private val shape: Shape,
        private val symbol: Symbol,
    ) : Runnable {
        private fun getJsonName(member: MemberShape): String {
            val jsonName = member.getTrait(JsonNameTrait::class.java)
            if (jsonName.isPresent) {
                return jsonName.get().value
            }
            return getConstructorName(member)
        }

        override fun run() {
            writer.pushState()
            writer.putContext("shape", symbol)
            writer.putContext("serializerClass", ToJSON)
            writer.openBlock("instance #{serializerClass:T} #{shape:T} where", "") {
                for (member in shape.members()) {
                    val jsonName = "\"${getJsonName(member)}\""
                    writer.pushState()
                    writer.putContext("constructor", getConstructorName(member))
                    writer.putContext("objectBuilder", JsonObjectBuilder)
                    writer.putContext("jsonName", jsonName)
                    writer.write("toJSON (#{constructor:L} a) = #{objectBuilder:T} [ #{jsonName:L} .= a ]")
                    writer.popState()
                }
            }
            writer.popState()
        }
    }
}
