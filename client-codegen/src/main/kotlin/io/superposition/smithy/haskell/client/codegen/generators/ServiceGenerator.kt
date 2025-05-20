@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.ServiceShape
import java.util.function.Consumer

class ServiceGenerator<
    T : ShapeDirective<ServiceShape, HaskellContext, HaskellSettings>
    > : Consumer<T> {
    override fun accept(directive: T) {
        val context = directive.context()
        val service = directive.service()
        val symbol = directive.symbol()

        // Generate service client code
        context.writerDelegator().useShapeWriter(service) { writer ->
            writer.write("data #T = #T", symbol, symbol)
            writer.addExport(symbol.name)
        }
    }
}
