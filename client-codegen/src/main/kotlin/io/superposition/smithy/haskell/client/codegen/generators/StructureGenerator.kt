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

    override fun run() {
        val symbolProvider = directive.symbolProvider()
        val shape = directive.shape()
        val symbol = directive.symbol()
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val record = Record(
                symbol.name,
                shape.members().map { Record.Field(it.memberName, symbolProvider.toSymbol(it)) }
            )
            writer.writeRecord(record)
            writer.addExport(symbol.name)
            shape.members().forEach {
                writer.addExport(it.memberName)
            }
            writer.exposeModule()
            writer.write("#C", BuilderGenerator(shape, symbol, symbolProvider, writer))
        }
    }
}
