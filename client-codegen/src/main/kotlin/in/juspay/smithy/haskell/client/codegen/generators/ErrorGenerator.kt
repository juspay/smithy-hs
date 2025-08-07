@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.HaskellShapeDirective
import software.amazon.smithy.model.shapes.StructureShape
import java.util.function.Consumer

@Suppress("MaximumLineLength")
class ErrorGenerator<T : HaskellShapeDirective<StructureShape>> : Consumer<T> {
    override fun accept(directive: T) {
        // Nothing special for Errors for now.
        StructureGenerator(directive).run()
    }
}
