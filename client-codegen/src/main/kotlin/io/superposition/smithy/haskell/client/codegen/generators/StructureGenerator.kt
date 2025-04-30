@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.StructureShape
import java.util.function.Consumer

class StructureGenerator<T : ShapeDirective<StructureShape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val shape = directive.shape()

        // Generate structure code
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            // Write structure implementation
            writer.write("-- Structure implementation for ${structure.id.name}")

            writer.write("data ${structure.id.name} = ${structure.id.name} {")
            for (member in shape.members()) {
                val memberName = member.memberName
                val memberType = directive.context().symbolProvider().toSymbol(member.target)
                writer.write("  $memberName :: ${memberType.name},")
            }
            writer.write("}")
        }
    }
}


