@file:Suppress(
    "FINITE_BOUNDS_VIOLATION_IN_JAVA",
    "UnusedPrivateMember",
    "UnusedPrivateProperty"
)

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.toHaskellHttpMethod
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Encoding
import io.superposition.smithy.haskell.client.codegen.Http.DefaultHttpManagerSettings
import io.superposition.smithy.haskell.client.codegen.Http.HttpClient
import io.superposition.smithy.haskell.client.codegen.Http.NewManager
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
    private val inputSymbol = symbolProvider.toSymbol(model.expectShape(opShape.inputShape))
    private val outputSymbol = symbolProvider.toSymbol(model.expectShape(opShape.outputShape))

    private val httpBindingIndex = HttpBindingIndex.of(model)
    private val requestBindings = httpBindingIndex.getRequestBindings(opShape)
    private val httpTrait = opShape.getTrait(HttpTrait::class.java).orElse(null)

    fun run() {
        val shape = directive.shape()

        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template = """
            -- Operation Error
            #{operationError:C|}

            #{operation:C|}

            #{operationRequestPayload:C|}

            #{operationRequestQueryParams:C|}

            #{operationRequestPath:C|}
            """.trimIndent()

            // todo push inputSymbol to context
            writer.pushState()
            writer.putContext("input", inputSymbol)
            writer.putContext(
                "operationError",
                Runnable { operationErrorGenerator(writer) }
            )
            writer.putContext(
                "operation",
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

    private fun inputBuilderSymbol(): Symbol {
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
            writer.write("${if (opShape.errors.isNotEmpty()) "| " else ""}BuilderError #{text:T}")
        }
    }

    private fun operationSignatureGenerator(writer: HaskellWriter) {
        val operationInput = inputBuilderSymbol()
        val operationOutput = outputSymbol.toEither(operationErrorSymbol(opSymbol)).inIO()

        // add service shape as a param to the operation
        writer.write("${opSymbol.name} :: #T -> #T", operationInput, operationOutput)
        writer.openBlock("${opSymbol.name} inputB = do", "") {
            val method = toHaskellHttpMethod(httpTrait.method)
            writer.write("let request = #{input:N}.build inputB <&> toRequest")
            writer.openBlock("case request of", "") {
                writer.write("#{left:T} err -> return (#{left:T} (BuilderError err))")
                writer.openBlock("#{right:T} req -> do", "") {
                    writer.write("response <- #T req (#T #T)", HttpClient, NewManager, DefaultHttpManagerSettings)
                    writer.write("return (#{right:T} response)")
                }
            }
            writer.openBlock("where", "") {
                if (method == Http.Custom) {
                    writer.write("method = \"${httpTrait.method}\"")
                } else {
                    writer.write("method = #T", method)
                }
                writer.openBlock("request input = #T {", "}", Http.Request) {
                    writer.write("#T = requestPath input", Http.rqPath)
                    writer.write("#T = method", Http.rqMethod)
                    writer.write("#T = requestQuery input", Http.rqHeaders)
                    writer.write("#T = requestPayload input", Http.rqBody)
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
        writer.write("requestPayload :: #{input:T} -> #{byteString:T}")
        if (payloadMemberName != null) {
            writer.write("requestPayload = encode")
        } else {
            val documentMembers = httpDocumentMembers()
            val varName = "input"
            writer.openBlock("requestPayload $varName =", "") {
                writer.openBlock("#{aeson:N}.encode $ #{aeson:N}.object", "") {
                    for ((i, member) in documentMembers.withIndex()) {
                        val mName = symbolProvider.toMemberName(member)
                        val jsonName =
                            member.getTrait(JsonNameTrait::class.java).map { it.value }
                                .orElse(mName)
                        if (i == 0) {
                            writer.writeInline("[ ")
                        } else {
                            writer.writeInline(", ")
                        }

                        writer.write("\"$jsonName\" .= #{input:N}.$mName $varName")
                    }
                    writer.write("]")
                }
            }
        }
    }

    @Suppress("LongMethod")
    private fun httpQueryParamsGenerator(writer: HaskellWriter) {
        val queryParams = httpQueryMembers()
        val mapParams = httpQueryParamMember().firstOrNull()
        val literalParams = httpTrait.uri.queryLiterals
        val preloadedParams = literalParams.keys + queryParams.map { it.locationName }

        writer.write("requestQuery :: #{input:T} -> #{byteString:T}")
        writer.openBlock("requestQuery input =", "") {
            if (queryParams.isEmpty() && mapParams != null && literalParams.isEmpty()) {
                writer.write("#{byteString:N}.empty")
            } else {
                writer.write(
                    "#{queryString:T}.toString (#{queryString:T}.queryString m)"
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
                            "mapParams = (#{map:N}.toList $ #{input:N}.$memberName input)",
                            "",
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
                                    "++ (#{list:N}.concatMap (\\x -> ($query, x)) (#{input:N}.$memberName input))"
                                )
                            } else {
                                writer.write(
                                    "++ [($query, (#{input:N}.$memberName input))]"
                                )
                            }
                        }
                    }

                    if (mapParams != null) {
                        writer.write(
                            "m = staticParams ++ mapParams ++ dynamicParams"
                        )
                    } else { writer.write("m = staticParams ++ dynamicParams") }
                }
            }
        }
    }

    private fun httpPathGenerator(writer: HaskellWriter) {
        // val labelBindings = requestBindings.filter { it.value.location == HttpBinding.Location.LABEL }
        //     .map { it.value }
        val uriSegments = httpTrait.uri.segments

        // Handle implement toString for each type

        writer.write("requestPath :: #{input:T} -> #{byteString:T}")
        writer.openBlock("requestPath input = ", "") {
            writer.write("#T.encodeUtf8 _path", Encoding)
            writer.openBlock("where", "") {
                writer.openBlock("_path = #{text:N}.empty", "") {
                    for (segment in uriSegments) {
                        if (segment.isLabel) {
                            val name = segment.content
                            writer.write("<> (#{input:N}.$name input)")
                        } else {
                            writer.write("<> \"${segment.content}\"")
                        }
                    }
                }
            }
        }
    }

    private fun httpResponseDeserializer(writer: HaskellWriter) {
        val responseSymbol = symbolProvider.toSymbol(model.expectShape(opShape.outputShape))
        val errorSymbol = operationErrorSymbol(opSymbol)
        val responseType = responseSymbol.toEither(errorSymbol).inIO()
    }
}
