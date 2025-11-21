@file:Suppress(
    "FINITE_BOUNDS_VIOLATION_IN_JAVA",
)

package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.*
import `in`.juspay.smithy.haskell.client.codegen.language.ClientRecord
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.*
import software.amazon.smithy.model.traits.HttpTrait
import kotlin.jvm.optionals.getOrNull

@Suppress("MaxLineLength")
class OperationGenerator<T : HaskellShapeDirective<OperationShape>>(
    private val directive: T,
) {
    private val opSymbol = directive.symbol()
    private val opShape = directive.shape()
    private val model = directive.model()
    private val httpBindingIndex = HttpBindingIndex.of(model)
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
        directive.symbolProvider(),
        directive.context(),
    )
    private val httpTrait = opShape.getTrait(HttpTrait::class.java).orElse(null)

    fun run() {
        val shape = directive.shape()
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template = """
            #{operationError:C|}

            $functionName :: #{client:T} -> #{input:T}Builder () -> IO (Either ${opSymbol.name}Error #{output:T})
            $functionName client builder =
                let endpoint = #{client:N}.endpointUri client
                    manager = #{client:N}.httpManager client
                    auth = #{client:N}.getAuth client
                in #{utility:N}.runOperation endpoint manager auth (#{input:N}.build builder)
            """.trimIndent()
            // TODO Delete un-used context.
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
                "httpMetadata",
                directive.context().utilitySymbol.toBuilder().name(
                    "HttpMetadata",
                ).build(),
            )
            writer.putContext(
                "operationError",
                OperationErrorGenerator(writer),
            )
            writer.write(template)
            writer.addExport(functionName)
            writer.exposeModule()
            writer.popState()
        }
        IntoRequestBuilderGenerator(
            model.expectShape(opShape.inputShape),
            symbolProvider,
            directive.context(),
            httpTrait,
            httpBindingIndex.determineRequestContentType(opShape, "application/json")
                .getOrNull(),
            httpBindingIndex.getRequestBindings(opShape),
        ).run()
        FromResponseParserGenerator(
            model.expectShape(opShape.outputShape),
            symbolProvider,
            directive.context(),
            httpTrait.code,
            httpBindingIndex.getResponseBindings(opShape),
        ).run()
    }

    inner class OperationErrorGenerator(val writer: HaskellWriter) : Runnable {
        private val operationErrorName = "${opSymbol.name}Error"

        override fun run() {
            writer.pushState()
            writer.putContext("struct", Runnable { generateStructure() })
            writer.putContext("getErrorParser", Runnable { errorParserGetter() })
            writer.write(
                """
                #{struct:C}
                instance #{aeson:N}.ToJSON $operationErrorName
                instance #{utility:N}.OperationError $operationErrorName where
                    mkBuilderError = BuilderError
                    mkDeSerializationError = DeSerializationError
                    mkUnexpectedError = UnexpectedError

                    #{getErrorParser:C|}
                """.trimIndent(),
            )
            writer.popState()
            writer.addExport("$operationErrorName (..)")
        }

        fun generateStructure() {
            writer.openBlock("data $operationErrorName =", "") {
                for ((i, errorShape) in opShape.errors.withIndex()) {
                    if (i != 0) {
                        writer.writeInline("| ")
                    }
                    writer.write(
                        "${errorShape.name} #T",
                        symbolProvider.toSymbol(model.expectShape(errorShape)),
                    )
                }
                writer.write(
                    "${if (opShape.errors.isNotEmpty()) "| " else ""}BuilderError #{text:T}",
                )
                writer.write("| DeSerializationError #{httpMetadata:T} #{text:T}")
                writer.write("| UnexpectedError (#{maybe:T} #{httpMetadata:T}) #{text:T}")
                // FIXME Not a good way to scale things out. We should be able to use `EnumGenerator` here
                // but it's too coupled to `Shape` for us to use it in a stable way right now. RIP.
                writer.write(
                    "   deriving (#T, #T)",
                    HaskellSymbol.Generic,
                    HaskellSymbol.Show,
                )
            }
        }

        private fun errorParserGetter() {
            writer.openBlock("getErrorParser status", "") {
                opShape.errors.forEach {
                    val symbol = symbolProvider.toSymbol(model.expectShape(it))
                    writer.write(
                        "| status == (#{utility:N}.expectedStatus @#T) = Just (fmap ${it.name} (#{utility:N}.responseParser @#T))",
                        symbol,
                        symbol,
                    )
                }
                writer.write("| otherwise = Nothing")
            }
        }
    }
}
