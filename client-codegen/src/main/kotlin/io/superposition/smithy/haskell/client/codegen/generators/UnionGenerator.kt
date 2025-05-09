@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.ToJSON
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.UnionShape
import software.amazon.smithy.utils.CaseUtils
import java.util.*
import java.util.function.Consumer

class UnionGenerator<T : ShapeDirective<UnionShape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val union = directive.shape()

        directive.context().writerDelegator().useShapeWriter(union) { writer ->
            val template = """
            -- Union implementation for #{shape:T}
            data #{shape:T} = 
                #{members:C|}
             
            #{serializer:C|}
            """.trimIndent()

            writer.pushState()
            writer.putContext("shape", directive.symbol())
            writer.putContext("members", MembersGenerator(writer, directive.symbolProvider(), union))
            writer.putContext("serializer", SerializerGenerator(writer, union, directive.symbol()))
            writer.write(template)
            writer.popState()
        }
    }

    private class MembersGenerator(
        private val writer: HaskellWriter,
        private val symbolProvider: SymbolProvider,
        private val shape: UnionShape,
    ) : Runnable {
        override fun run() {
            for ((i, member) in shape.members().withIndex()) {
                writer.pushState()
                writer.putContext(
                    "name",
                    CaseUtils.toPascalCase(member.memberName)
                )
                writer.putContext("type", symbolProvider.toSymbol(member))
                if (i == 0) writer.write("#{name:L} #{type:T}")
                else writer.write("| #{name:L} #{type:T}")
                writer.popState()
            }
        }
    }

    private class SerializerGenerator(
        private val writer: HaskellWriter,
        private val shape: Shape,
        private val symbol: Symbol,
    ) : Runnable {
        override fun run() {
            writer.pushState()
            writer.putContext("shape", symbol)
            writer.putContext("serializerClass", ToJSON)
            writer.openBlock("instance #{serializerClass:T} #{shape:T} where", "") {
                for (member in shape.members()) {
                    writer.pushState()
                    writer.putContext("name", CaseUtils.toPascalCase(member.memberName))
                    writer.write("toJSON #{name:L} a = toJSON a")
                    writer.popState()
                }
            }
            writer.popState()
        }
    }
}