@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.HaskellShapeDirective
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.StructureShape
import java.util.function.Consumer

@Suppress("MaximumLineLength")
class ErrorGenerator<T : HaskellShapeDirective<StructureShape>> : Consumer<T> {
    override fun accept(directive: T) {
        StructureGenerator(directive).run()
        val httpBindingIndex = HttpBindingIndex.of(directive.model())
        FromResponseParserGenerator(
            directive.shape(),
            directive.symbolProvider(),
            directive.context(),
            httpBindingIndex.getResponseCode(directive.shape()),
            httpBindingIndex.getResponseBindings(directive.shape()),
        ).run()
    }
}
