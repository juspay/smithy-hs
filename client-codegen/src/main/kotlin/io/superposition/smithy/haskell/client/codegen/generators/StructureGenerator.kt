@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Generic
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.StructureShape
import java.util.function.Consumer

@Suppress("MaxLineLength")
class StructureGenerator<T : ShapeDirective<StructureShape, HaskellContext, HaskellSettings>> :
    Consumer<T> {
    override fun accept(directive: T) {
        val shape = directive.shape()

        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template = """
            -- Structure implementation for #{shape:T}
            data #{shape:T} = #{shape:T}
            {
                #{properties:C|}
            } #{derives:C|}
            
            #{serializer:C|}
            """.trimIndent()
            writer.pushState()
            writer.putContext("shape", directive.symbol())
            writer.putContext(
                "properties",
                PropertyGenerator(writer, shape, directive.symbolProvider(), directive.context().model)
            )
            writer.putContext("derives", DerivesGenerator(writer))
            writer.putContext("serializer", StructureSerializerGenerator(directive.context(), shape))
            writer.write(template)
            writer.popState()
        }
    }

    private class PropertyGenerator(
        private val writer: HaskellWriter,
        private val shape: StructureShape,
        private val symbolProvider: SymbolProvider,
        private val model: Model
    ) : Runnable {
        override fun run() {
            for (member in shape.members()) {
                val memberName = symbolProvider.toMemberName(member)
                val memberType = symbolProvider.toSymbol(member)
                writer.write("$memberName :: #T,", memberType)
            }
        }
    }

    private class DerivesGenerator(private val writer: HaskellWriter) : Runnable {
        val defaultDerives = listOf(Generic)
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
