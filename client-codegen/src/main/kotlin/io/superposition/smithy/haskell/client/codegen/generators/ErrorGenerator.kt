@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.language.Record
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.StructureShape
import java.util.function.Consumer

// NOTE: we should probably use StructureGenerator for generating code for errors
@Suppress("MaximumLineLength")
class ErrorGenerator<T : ShapeDirective<StructureShape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val context = directive.context()
        val error = directive.shape()
        val symbol = directive.symbol()
        val symbolProvider = directive.symbolProvider()

        context.writerDelegator().useShapeWriter(error) { writer ->
            val record = Record(
                symbol.name,
                error.members().map { Record.Field(it.memberName, symbolProvider.toSymbol(it)) }
            )
            writer.writeRecord(record)
            writer.addExport(record.name)
        }
    }
}
