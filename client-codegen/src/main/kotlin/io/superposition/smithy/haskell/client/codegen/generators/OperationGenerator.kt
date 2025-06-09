@file:Suppress(
    "FINITE_BOUNDS_VIOLATION_IN_JAVA",
)

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.language.ClientRecord
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.model.shapes.*

@Suppress("MaxLineLength")
class OperationGenerator<T : HaskellShapeDirective<OperationShape>>(
    private val directive: T
) {
    private val opSymbol = directive.symbol()
    private val opShape = directive.shape()
    private val model = directive.model()
    private val symbolProvider = directive.symbolProvider()
    private val inputSymbol =
        symbolProvider.toSymbol(model.expectShape(opShape.inputShape))
    private val outputSymbol =
        symbolProvider.toSymbol(model.expectShape(opShape.outputShape))

    private val functionName = opSymbol.name.replaceFirstChar { it.lowercase() }
    private val outputBuilder = Symbol.builder().name("${outputSymbol.name}Builder")
        .namespace(outputSymbol.namespace, ".")
        .build()

    private val service =
        model.expectShape(directive.settings().service, ServiceShape::class.java)
    private val clientSymbol = symbolProvider.toSymbol(service)

    private val client = ClientRecord(
        service,
        directive.symbolProvider()
    )

    fun run() {
        val shape = directive.shape()
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template = """
            #{operationError:C|}

            #{request:C|}

            #{responseDeserializer:C|}
            """.trimIndent()

            // todo push inputSymbol to context
            writer.pushState()
            writer.putContext("input", inputSymbol)
            writer.putContext("output", outputSymbol)
            writer.putContext("outputBuilder", outputBuilder)
            writer.putContext("client", clientSymbol)
            writer.putContext("uri", HaskellSymbol.Http.Uri)
            writer.putContext("ci", HaskellSymbol.Misc.CaseInsensitive)
            writer.putContext("someException", HaskellSymbol.SomeException)
            writer.putContext("encoding", HaskellSymbol.EncodingUtf8)
            writer.putContext("utility", directive.context().utilitySymbol)
            writer.putContext(
                "operationError",
                Runnable { operationErrorGenerator(writer) }
            )
            writer.putContext(
                "request",
                RequestBindingGenerator(opShape, model, writer, symbolProvider, client)
            )
            writer.putContext(
                "responseDeserializer",
                ResponseBindingGenerator(
                    opShape,
                    model,
                    writer,
                    symbolProvider,
                )
            )
            writer.write(template)
            writer.addExport(functionName)
            writer.exposeModule()
            writer.popState()
        }
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
            writer.write("${if (opShape.errors.isNotEmpty()) "| " else ""}BuilderError #{text:T}")
            writer.write("| RequestError #{text:T}")
        }
        writer.addExport("$operationErrorName(..)")
    }
}
