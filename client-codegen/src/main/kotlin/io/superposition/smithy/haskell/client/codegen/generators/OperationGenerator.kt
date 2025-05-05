@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.OperationShape
import java.util.function.Consumer

class OperationGenerator<T : ShapeDirective<OperationShape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val shape = directive.shape()

        // Generate operation code
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            // Write operation implementation
            val input = directive.symbolProvider().toSymbol(directive.model().expectShape(shape.inputShape))
            val output = directive.symbolProvider().toSymbol(directive.model().expectShape(shape.outputShape))

            writer.write("-- Operation implementation for ${shape.id.name}")
            writer.write("${shape.id.name} :: #T -> #T", input, output)
        }
    }
}
