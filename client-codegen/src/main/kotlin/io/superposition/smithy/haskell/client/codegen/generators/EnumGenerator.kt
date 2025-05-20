@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.Shape
import java.util.function.Consumer

class EnumGenerator<T : ShapeDirective<Shape, HaskellContext, HaskellSettings>> : Consumer<T> {
    override fun accept(directive: T) {
        val shape = directive.shape()
        val symbol = directive.symbol()

        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val members = shape.members()
            writer.openBlock("data ${symbol.name} =", "") {
                writer.write("  ${members.first().memberName}")
                members.drop(1).forEach {
                    writer.write("| ${it.memberName}")
                }
            }
            writer.addExport(symbol.name)
        }
    }
}
