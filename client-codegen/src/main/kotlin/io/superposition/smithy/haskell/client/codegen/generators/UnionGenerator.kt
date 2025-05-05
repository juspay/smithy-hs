@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.UnionShape
import java.util.function.Consumer

class UnionGenerator<T : ShapeDirective<UnionShape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val context = directive.context()
        val union = directive.shape()

        context.writerDelegator().useShapeWriter(union) { writer ->
            // Write union implementation
            writer.write("-- Union for ${union.id.name}")
            writer.write("data ${union.id.name}")
            writer.write(" = ")

            // Add union members
            union.members().forEachIndexed { index, member ->
                val s = context.symbolProvider().toSymbol(member)
                val prefix = if (index == 0) "" else "  | "
                writer.write("${prefix}${member.memberName} ${s.name}")
            }

            writer.write("  deriving (Show, Eq)")
        }
    }
}
