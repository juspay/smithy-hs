@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.StructureShape
import java.util.function.Consumer

// NOTE: we should probably use StructureGenerator for generating code for errors
class ErrorGenerator<T : ShapeDirective<StructureShape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val context = directive.context()
        val error = directive.shape()

        context.writerDelegator().useShapeWriter(error) { writer ->
            // Write error implementation
            writer.write("-- Error for ${error.id.name}")
            writer.write("data ${error.id.name} = ${error.id.name}")
            writer.write("  { ")

            // Add error members
            error.members().forEach { member ->
                val memberSymbol = context.symbolProvider().toSymbol(member)
                writer.write("  ${member.memberName} :: ${memberSymbol.name}")
            }

            writer.write("  } deriving (Show, Eq)")
            writer.write("")
            writer.write("instance Exception ${error.id.name}")
        }
    }
}
