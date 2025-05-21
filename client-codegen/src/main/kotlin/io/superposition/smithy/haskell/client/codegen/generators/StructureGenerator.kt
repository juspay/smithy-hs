@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.language.Record
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.StructureShape

@Suppress("MaxLineLength")
class StructureGenerator<T : ShapeDirective<StructureShape, HaskellContext, HaskellSettings>>(
    private val directive: T
) : Runnable {
    private val shape = directive.shape()
    private val symbol = directive.symbol()
    private val symbolProvider = directive.symbolProvider()

    override fun run() {
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template = """
            #{record:C|}

            #{serializer:C|}

            #{builder:C|}
            """.trimIndent()

            val record = Record(
                symbol.name,
                shape.members().map { Record.Field(it.memberName, symbolProvider.toSymbol(it)) }
            )

            writer.pushState()
            writer.putContext("record", Runnable { writer.writeRecord(record) })
            writer.putContext(
                "serializer",
                StructureSerializerGenerator(shape.members(), symbol, symbolProvider, writer)
            )
            writer.putContext("builder", BuilderGenerator(record, symbol, writer))
            writer.write(template)
            writer.addExport(symbol.name)
            shape.members().forEach {
                writer.addExport(it.memberName)
            }
            writer.exposeModule()
            writer.popState()
        }
    }
}
