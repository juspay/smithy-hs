@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.HaskellShapeDirective
import `in`.juspay.smithy.haskell.client.codegen.fieldName
import `in`.juspay.smithy.haskell.client.codegen.language.Record
import software.amazon.smithy.model.shapes.StructureShape
import java.util.function.Consumer

// NOTE: we should probably use StructureGenerator for generating code for errors
@Suppress("MaximumLineLength")
class ErrorGenerator<T : HaskellShapeDirective<StructureShape>> : Consumer<T> {
    override fun accept(directive: T) {
        val context = directive.context()
        val error = directive.shape()
        val symbol = directive.symbol()
        val symbolProvider = directive.symbolProvider()

        context.writerDelegator().useShapeWriter(error) { writer ->
            val record = Record(
                symbol.name,
                error.members()
                    .map {
                        Record.Field(
                            it.fieldName,
                            symbolProvider.toSymbol(it)
                        )
                    }
            )
            writer.writeRecord(record)
            writer.addExport(record.name)
        }
    }
}
