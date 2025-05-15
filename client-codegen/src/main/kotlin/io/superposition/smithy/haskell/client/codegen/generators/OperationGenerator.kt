@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.ByteString
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.OperationShape

@Suppress("MaxLineLength", "ktlint:standard:max-line-length")
class OperationGenerator<T : ShapeDirective<OperationShape, HaskellContext, HaskellSettings>>(
    private val directive: T
) {
    private val opSymbol = directive.symbol()
    private val opShape = directive.shape()
    private val model = directive.model()
    private val symbolProvider = directive.symbolProvider()

    private val httpBindingIndex = HttpBindingIndex.of(model)
    private val requestBindings = httpBindingIndex.getRequestBindings(opShape)

    fun run() {
        val shape = directive.shape()

        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template = """
            -- Operation Error
            #{operationError:C|}

            #{operationSignature:C|}
            #{operationDefinition:C|}
            """.trimIndent()

            writer.pushState()
            writer.putContext(
                "operationError",
                Runnable { operationErrorGenerator(writer) }
            )
            writer.putContext(
                "operationSignature",
                Runnable { operationSignatureGenerator(writer) }
            )
            writer.write(template)
            writer.addExport(opSymbol.name)
            writer.popState()
        }
    }

    private fun inputBuilderSymbol(inputSymbol: Symbol): Symbol {
        return Symbol.builder().name("${inputSymbol.name}Builder").namespace(inputSymbol.namespace, ".")
            .build()
    }

    private fun operationErrorSymbol(opSymbol: Symbol): Symbol {
        return Symbol.builder().name("${opSymbol.name}Error").namespace(opSymbol.namespace, ".")
            .build()
    }

    private fun operationErrorGenerator(writer: HaskellWriter) {
        val operationErrorName = "${opSymbol.name}Error"
        writer.openBlock("data $operationErrorName =", "") {
            for ((i, errorShape) in opShape.errors.withIndex()) {
                if (i != 0) {
                    writer.writeInline("| ")
                }
                writer.write(
                    "${errorShape.name} #T",
                    symbolProvider.toSymbol(model.expectShape(errorShape))
                )
            }
        }
    }

    private fun operationSignatureGenerator(writer: HaskellWriter) {
        val inputSymbol = symbolProvider.toSymbol(model.expectShape(opShape.inputShape))
        val outputSymbol = symbolProvider.toSymbol(model.expectShape(opShape.outputShape))

        val operationInput = inputBuilderSymbol(inputSymbol)
        val operationOutput = outputSymbol.toEither(operationErrorSymbol(opSymbol)).inIO()

        writer.write("${opSymbol.name} :: #T -> #T", operationInput, operationOutput)
    }

    /***
     * URL params
     * Query params
     * payload
     * headers
     * non-payload members of the input
     */

    private fun httpPayloadMemberName(): String? {
        return requestBindings.filter { it.value.location == HttpBinding.Location.PAYLOAD }
            .map { it.key }
            .firstOrNull()
    }

    private fun httpPayloadGenerator(writer: HaskellWriter) {
        val payloadMemberName = httpPayloadMemberName()
        val inputSymbol = symbolProvider.toSymbol(model.expectShape(opShape.inputShape))
        writer.openBlock("requestPayload :: #T -> #T", "", inputSymbol, ByteString) {
            if (payloadMemberName != null) {
                writer.write("#T ")
            } else {
                // val payloadMember = opShape.inputShape(model).expectMember// (payloadMemberName)
                // writer.pushState()
                // writer.putContext("payloadMember", payloadMember)
                // writer.putContext("payloadType", symbolProvider.toSymbol// (payloadMember))
                // writer.write("Just #{payloadType:T} inputBuilder// .#L", payloadMember.memberName)
                // writer.popState()
            }
        }
    }

    private fun operationRequestGenerator(writer: HaskellWriter) {
        writer.openBlock("toRequest :: ")
    }

    private fun operationDefinitionGenerator(writer: HaskellWriter) {
        writer.openBlock("${opSymbol.name} inputBuilder = do", "") {
            writer.write("let input = build inputBuilder")
        }
    }
}
