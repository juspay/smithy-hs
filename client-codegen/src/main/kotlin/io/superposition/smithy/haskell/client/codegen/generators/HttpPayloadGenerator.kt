@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.inputShape
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.OperationShape

@Suppress("MaxLineLength")
class HttpPayloadGenerator<T : ShapeDirective<OperationShape, HaskellContext, HaskellSettings>> (
    private val directive: T
) {
    private val httpBindingIndex = HttpBindingIndex.of(directive.model())
    private val requestBindings = httpBindingIndex.getRequestBindings(directive.shape())

    private fun getPayloadMemberName(): String? {
        return requestBindings.filter { it.value.location == HttpBinding.Location.PAYLOAD }
            .map { it.key }
            .firstOrNull()
    }

    fun generateRequestPayload() {
        val operation = directive.shape()
        val inputShape = operation.inputShape(directive.model())
        // StructureSerializerGenerator(directive.context(), inputShape).generate()

        // val payloadMemberName = getPayloadMemberName()
        // if (payloadMemberName == null) {
        // } else {
        //     val payloadMember = operation.inputShape(directive.model())// .expectMember(payloadMemberName)
        // }
    }

    // private fun generatePayloadMember(member: MemberShape) {
    //     directive.context().writerDelegator().useShapeWriter(member) { writer ->
    //         val symbolProvider = directive.symbolProvider()
    //         val memberType = symbolProvider.toSymbol(member)
    //         writer.write("-- ToJSON implementation for ${memberType// .name}")
    //         writer.write("instance ToJSON ${memberType.name}")
    //     }
    // }
}
