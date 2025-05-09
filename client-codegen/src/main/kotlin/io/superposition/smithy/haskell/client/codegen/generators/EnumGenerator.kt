@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Eq
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Generic
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.JsonString
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.TextPack
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.ToJSON
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.traits.EnumValueTrait
import java.util.function.Consumer

class EnumGenerator<T : ShapeDirective<Shape, HaskellContext, HaskellSettings>> : Consumer<T> {
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
            """.trimIndent()

            writer.pushState()
            writer.putContext("shape", directive.symbol())
            writer.putContext("constructors", ConstructorGenerator(writer, shape))
            writer.putContext("derives", DerivesGenerator(writer))
            writer.putContext("serializer", SerializerGenerator(writer, shape, directive.symbol()))
            writer.write(template)
            writer.addExport(symbol.name)
            writer.popState()
        }
    }

    private class ConstructorGenerator(
        private val writer: HaskellWriter,
        private val shape: Shape,
    ) : Runnable {
        override fun run() {
            for ((i, member) in shape.members().withIndex()) {
                if (i == 0) writer.write(member.memberName)
                else writer.write("| ${member.memberName}")
            }
        }
    }

    private class SerializerGenerator(
        private val writer: HaskellWriter,
        private val shape: Shape,
        private val symbol: Symbol,
    ) : Runnable {
        private fun getJsonName(member: MemberShape): String {
            val enumValue = member.getTrait(EnumValueTrait::class.java)
            if (enumValue.isPresent) {
                return enumValue.get().expectStringValue()
            }
            return member.memberName
        }

        override fun run() {
            writer.pushState()
            writer.putContext("shape", symbol)
            writer.putContext("serializerClass", ToJSON)
            writer.putContext("jsonString", JsonString)
            writer.putContext("textPack", TextPack)
            writer.openBlock("instance #{serializerClass:T} #{shape:T} where", "") {
                for (member in shape.members()) {
                    val jsonName = getJsonName(member)
                    val constructor = member.memberName
                    writer.pushState()
                    writer.putContext("constructor", constructor)
                    writer.putContext("jsonName", "\"${jsonName}\"")
                    writer.write("toJSON #{constructor:L} = #{jsonString:T} $ #{textPack:T} #{jsonName:L}")
                    writer.popState()
                }
            }
            writer.popState()
        }
    }

    private class DerivesGenerator(private val writer: HaskellWriter) : Runnable {
        val defaultDerives = listOf(Generic, Eq)
        override fun run() {
            writer.writeInline("deriving (")
            for ((i, derive) in defaultDerives.withIndex()) {
                if (i == 0) writer.writeInline("#T", derive)
                else writer.writeInline(", #T", derive)
            }
            writer.writeInline(")")
        }
    }
}
