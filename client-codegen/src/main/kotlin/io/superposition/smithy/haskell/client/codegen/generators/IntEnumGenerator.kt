@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.Shape
import java.util.function.Consumer

class IntEnumGenerator<T : ShapeDirective<Shape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val shape = directive.shape()

        // Generate operation code
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            // Write operation implementation
            writer.write("-- IntEnum implementation for ${shape.id.name}")
            writer.write("-- TODO: Implement IntEnum")
        }
    }
}
