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
        val symbolProvider = directive.symbolProvider()

        // Generate structure code
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            // Write structure implementation
            writer.write("-- Structure implementation for ${shape.id.name}")

            writer.write("data ${shape.id.name} = ${shape.id.name} {")
            for (member in shape.members()) {
                // TODO check for string symbols
                val memberName = symbolProvider.toMemberName(member)
                val memberType = symbolProvider.toSymbol(member)
                writer.write("  $memberName :: #T,", memberType)
            }
            writer.write("}")
        }
    }
}
