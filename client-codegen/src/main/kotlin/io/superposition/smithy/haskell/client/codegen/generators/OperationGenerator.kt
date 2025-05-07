@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.OperationShape
import java.util.function.Consumer

@Suppress("MaxLineLength", "ktlint:standard:max-line-length")
class OperationGenerator<T : ShapeDirective<OperationShape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val shape = directive.shape()

        // Generate operation code
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            HttpPayloadGenerator(directive).generateRequestPayload()
            // Write operation implementation
            val input = directive.symbolProvider().toSymbol(
                directive.model().expectShape(shape.inputShape)
            )
            val output = directive.symbolProvider().toSymbol(
                directive.model().expectShape(shape.outputShape)
            )

            val symbol = directive.symbol()
            writer.write("#T :: ()", symbol)
            writer.write("#T = ()", symbol)
            writer.addExport(symbol.name)
        }
    }
}
