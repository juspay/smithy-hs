@file:Suppress(
    "FINITE_BOUNDS_VIOLATION_IN_JAVA",
    "UnusedPrivateMember",
    "UnusedPrivateProperty"
)

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.toHaskellHttpMethod
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.ByteString
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Encoding
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.JsonEncode
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.JsonObjectBuilder
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Text
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.MapShape
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.traits.HttpTrait
import software.amazon.smithy.model.traits.JsonNameTrait

@Suppress(
    "MaxLineLength",
    "ktlint:standard:max-line-length",
    "TooManyFunctions",
    "LargeClass"
)
class OperationGenerator<T : ShapeDirective<OperationShape, HaskellContext, HaskellSettings>>(
    private val directive: T
) {
    private val opSymbol = directive.symbol()
    private val opShape = directive.shape()
    private val model = directive.model()
    private val symbolProvider = directive.symbolProvider()

    private val httpBindingIndex = HttpBindingIndex.of(model)
    private val requestBindings = httpBindingIndex.getRequestBindings(opShape)
    private val httpTrait = opShape.getTrait(HttpTrait::class.java).orElse(null)

    fun run() {
        val shape = directive.shape()

        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            // assert(writer.getContext("QueryString") != null)
            // println(writer.getContext("QueryString"))
            val template = """
            -- Operation Error
            #{operationError:C|}

            #{operationSignature:C|}

            #{operationRequestPayload:C|}

            #{operationRequestQueryParams:C|}

            #{operationRequestPath:C|}
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
            writer.putContext(
                "operationRequestPayload",
                Runnable { httpPayloadGenerator(writer) }
            )
            writer.putContext(
                "operationRequestQueryParams",
                Runnable { httpQueryParamsGenerator(writer) }
            )
            writer.putContext(
                "operationRequestPath",
                Runnable { httpPathGenerator(writer) }
            )
            writer.write(template)
            writer.addExport(opSymbol.name)
            writer.popState()
        }
    }

    private fun inputBuilderSymbol(inputSymbol: Symbol): Symbol {
        return Symbol.builder().name("${inputSymbol.name}Builder")
            .namespace(inputSymbol.namespace, ".")
            .build()
    }

    private fun operationErrorSymbol(opSymbol: Symbol): Symbol {
        return Symbol.builder().name("${opSymbol.name}Error")
            .namespace(opSymbol.namespace, ".")
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
        writer.openBlock("${opSymbol.name} input = do", "") {
            val method = toHaskellHttpMethod(httpTrait.method)
            writer.write("result <- #T request", HaskellHttp.SimpleHttpClient)
            writer.openBlock("where", "") {
                writer.write("path = requestPath input")
                writer.write("query = requestQuery input")
                writer.write("payload = requestPayload input")
                if (method == HaskellHttp.Custom) {
                    writer.write("method = #T ${httpTrait.method}", method)
                } else {
                    writer.write("method = #T", method)
                }
                writer.openBlock("request = #T {", "}", HaskellHttp.Request) {
                    writer.write("#T = path", HaskellHttp.rqURI)
                    writer.write("#T = method", HaskellHttp.rqMethod)
                    writer.write("#T = query", HaskellHttp.rqHeaders)
                    writer.write("#T = payload", HaskellHttp.rqBody)
                }
            }
        }
    }

    /***
     * URL: Done
     * Query params: Done
     * payload: Done
     * headers
     * non-payload members of the input: Done
     * Make the api call
     */

    private fun httpPayloadMemberName(): String? {
        return requestBindings.filter { it.value.location == HttpBinding.Location.PAYLOAD }
            .map { it.key }
            .firstOrNull()
    }

    private fun httpQueryParamMember(): List<HttpBinding> {
        return requestBindings.filter { it.value.location == HttpBinding.Location.QUERY_PARAMS }
            .map { it.value }
    }

    private fun httpQueryMembers(): List<HttpBinding> {
        return requestBindings.filter { it.value.location == HttpBinding.Location.QUERY }
            .map { it.value }
    }

    private fun httpDocumentMembers(): List<MemberShape> {
        return requestBindings.filter { it.value.location == HttpBinding.Location.DOCUMENT }
            .map { it.value.member }
    }

    private fun httpHeadersMembers(): List<HttpBinding> {
        return requestBindings.filter { it.value.location == HttpBinding.Location.HEADER }
            .map { it.value }
    }

    private fun httpHeadersPrefixMembers(): List<HttpBinding> {
        return requestBindings.filter { it.value.location == HttpBinding.Location.PREFIX_HEADERS }
            .map { it.value }
    }

    private fun httpPayloadGenerator(writer: HaskellWriter) {
        val payloadMemberName = httpPayloadMemberName()
        val inputSymbol = symbolProvider.toSymbol(model.expectShape(opShape.inputShape))
        writer.write("requestPayload :: #T -> #T", inputSymbol, ByteString)
        if (payloadMemberName != null) {
            writer.write("requestPayload = encode")
        } else {
            val documentMembers = httpDocumentMembers()
            writer.openBlock("requestPayload input =", "") {
                writer.openBlock("#T $ #T [", "]", JsonEncode, JsonObjectBuilder) {
                    for (member in documentMembers) {
                        val memberName = symbolProvider.toMemberName(member)
                        val jsonName =
                            member.getTrait(JsonNameTrait::class.java).map { it.value }
                                .orElse(memberName)
                        writer.write("\"$jsonName\" .= $memberName input")
                    }
                }
            }
        }
    }

    @Suppress("LongMethod")
    private fun httpQueryParamsGenerator(writer: HaskellWriter) {
        val queryParams = httpQueryMembers()
        val mapParams = httpQueryParamMember().firstOrNull()
        val literalParams = httpTrait.uri.queryLiterals

        val inputSymbol = symbolProvider.toSymbol(model.expectShape(opShape.inputShape))
        val preloadedParams = literalParams.keys + queryParams.map { it.locationName }

        writer.write("requestQuery :: #T -> #T", inputSymbol, ByteString)
        writer.openBlock("requestQuery input =", "") {
            if (queryParams.isEmpty() && mapParams != null && literalParams.isEmpty()) {
                writer.write("#T.empty", ByteString)
            } else {
                writer.write(
                    "#{queryString:N}.toString (#{queryString:N}.queryString m)"
                )
                writer.openBlock("where", "") {
                    if (mapParams != null) {
                        writer.openBlock("reservedParams = [", "]") {
                            for ((i, param) in preloadedParams.withIndex()) {
                                if (i != 0) {
                                    writer.writeInline(", ")
                                }
                                writer.write("\"$param\"")
                            }
                        }

                        val memberShape = mapParams.member
                        val memberName = symbolProvider.toMemberName(memberShape)
                        val target = model.expectShape(
                            memberShape?.target,
                            MapShape::class.java
                        )

                        writer.openBlock(
                            "mapParams = (#{map:N}.toList $ #T.$memberName input)",
                            "",
                            inputSymbol
                        ) {
                            writer.write("& (#{list:N}.filter (\\(k, _) -> #{list:N}.find (== k) reservedParams))")
                            if (target.value.isListShape) {
                                writer.write("& (#{list:N}.concatMap (\\(k, v) -> #{list:N}.map (\\x -> (k, x)) v))")
                            }
                        }
                    }

                    writer.openBlock("staticParams = []", "") {
                        for ((k, v) in literalParams) {
                            writer.write("++[(\"$k\", \"$v\")]")
                        }
                    }

                    writer.openBlock("dynamicParams = []", "") {
                        for (param in queryParams) {
                            val memberShape = param.member
                            val memberName = symbolProvider.toMemberName(memberShape)
                            val target = model.expectShape(
                                memberShape?.target
                            )

                            val query = "\"${param.locationName}\""
                            if (target.isListShape) {
                                writer.write(
                                    "++ (#{list:N}.concatMap (\\x -> ($query, x)) (#T.$memberName input))",
                                    inputSymbol
                                )
                            } else {
                                writer.write(
                                    "++ [($query, (#T.$memberName input))]",
                                    inputSymbol
                                )
                            }
                        }
                    }

                    if (mapParams != null) {
                        writer.write("m = staticParams ++ mapParams ++ dynamicParams")
                    } else {
                        writer.write("m = staticParams ++ dynamicParams")
                    }
                }
            }
        }
    }

    private fun httpPathGenerator(writer: HaskellWriter) {
        // val labelBindings = requestBindings.filter { it.value.location == HttpBinding.Location.LABEL }
        //     .map { it.value }
        val inputSymbol = symbolProvider.toSymbol(model.expectShape(opShape.inputShape))
        val uriSegments = httpTrait.uri.segments

        // Handle implement toString for each type

        writer.write("requestPath :: #T -> #T", inputSymbol, ByteString)
        writer.openBlock("requestPath input = ", "") {
            writer.write("#T.encodeUtf8 $ #T.pack _path", Encoding, Text)
            writer.openBlock("where", "") {
                writer.openBlock("_path = #T.empty", "", Text) {
                    for (segment in uriSegments) {
                        if (segment.isLabel) {
                            val name = segment.content
                            writer.write("<> (#T.$name input)", inputSymbol)
                        } else {
                            writer.write("<> \"${segment.content}\"")
                        }
                    }
                }
            }
        }
    }
}
