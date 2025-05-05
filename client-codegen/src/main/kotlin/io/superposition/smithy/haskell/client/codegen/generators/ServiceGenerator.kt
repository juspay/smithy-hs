@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.ServiceShape
import java.util.function.Consumer

class ServiceGenerator<T : ShapeDirective<ServiceShape, HaskellContext, HaskellSettings>> : Consumer<T> {

    override fun accept(directive: T) {
        val context = directive.context()
        val service = directive.service()
        val settings = directive.settings()

        // Generate service client code
        context.writerDelegator().useShapeWriter(service) { writer ->
            // Write service client implementation
            writer.write("-- Service client for ${service.id.name}")
            writer.write("module ${settings.packageName}.${service.id.name} where")
            writer.write("")
            writer.write("-- TODO: Implement service client")
        }
    }
}
