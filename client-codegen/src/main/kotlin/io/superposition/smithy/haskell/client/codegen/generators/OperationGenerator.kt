@file:Suppress(
    "FINITE_BOUNDS_VIOLATION_IN_JAVA",
)

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.toHaskellHttpMethod
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.EncodingUtf8
import io.superposition.smithy.haskell.client.codegen.language.ClientRecord
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.MapShape
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.traits.HttpTrait
import software.amazon.smithy.model.traits.JsonNameTrait

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

    private val httpBindingIndex = HttpBindingIndex.of(model)
    private val requestBindings = httpBindingIndex.getRequestBindings(opShape)
    private val httpTrait = opShape.getTrait(HttpTrait::class.java).orElse(null)

    private val functionName = opSymbol.name.replaceFirstChar { it.lowercase() }

    private val service =
        model.expectShape(directive.settings().service, ServiceShape::class.java)
    val clientSymbol = symbolProvider.toSymbol(service)

    private val client = ClientRecord(
        service,
        directive.symbolProvider()
    )

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
            writer.putContext("client", clientSymbol)
            writer.putContext("uri", HaskellSymbol.Http.Uri)
            writer.putContext("someException", HaskellSymbol.SomeException)
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
            writer.addExport(functionName)
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
            writer.write("| RequestError #{text:T}")
        }
    }

    private fun operationSignatureGenerator(writer: HaskellWriter) {
        val operationInput = inputBuilderSymbol()
        // val operationOutput = outputSymbol.toEither(operationErrorSymbol(opSymbol)).inIO()
        val operationOutput =
            operationErrorSymbol(opSymbol)
                .toEither(outputSymbol.toBuilder().name("()").namespace("", ".").build())
                .inIO()

        writer.write(
            "$functionName :: #{client:T} -> #T () -> #T",
            operationInput,
            operationOutput
        )
        writer.openBlock("$functionName client inputB = do", "") {
            // #{functor:N}.<&> toRequest
            val method = toHaskellHttpMethod(httpTrait.method)
            writer.openBlock("let inputE = #{input:N}.build inputB", "") {
                writer.write("baseUri = #{client:N}.${client.uri.name} client")
                client.token?.let {
                    writer.write("token = #{client:N}.${it.name} client")
                }
                writer.write("httpManager = #{client:N}.${client.httpManager.name} client")
                writer.write(
                    "requestE = #{httpClient:N}.requestFromURI @(#{either:T} #{someException:T}) baseUri"
                )
            }
            writer.openBlock("case (inputE, requestE) of", "") {
                writer.write("(#{left:T} err, _) -> return $ #{left:T} (BuilderError err)")
                writer.write("(_, #{left:T} err) -> return $ #{left:T} (RequestError $ #{text:N}.pack $ show err)")
                writer.openBlock("(#{right:T} input, #{right:T} req) -> do", "") {
                    writer.write("response <- #{httpClient:T} (toRequest input req) httpManager")
                    writer.write("return (#{right:T} ())")
                }
            }
            writer.openBlock("where", "") {
                if (method == Http.Custom) {
                    writer.write("method = \"${httpTrait.method}\"")
                } else {
                    writer.write("method = #T", method)
                }
                writer.openBlock("toRequest input req =", "") {
                    writer.openBlock(
                        "let path = (#T req) <> (requestPath input)",
                        "",
                        Http.rqPath
                    ) {
                        writer.openBlock("in req {", "}") {
                            writer.write("#T = path,", Http.rqPath)
                            writer.write("#T = method,", Http.rqMethod)
                            writer.write("#T = requestQuery input,", Http.rqQueryString)
                            writer.write("#T = requestPayload input", Http.rqBody)
                        }
                    }
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
        writer.write("requestPayload :: #{input:T} -> #{httpClient:N}.RequestBody")
        if (payloadMemberName != null) {
            writer.write("requestPayload = encode")
        } else {
            val documentMembers = httpDocumentMembers()
            val varName = "input"
            writer.openBlock("requestPayload $varName =", "") {
                writer.openBlock(
                    "#{httpClient:N}.RequestBodyLBS $ #{aeson:N}.encode $ #{aeson:N}.object",
                    ""
                ) {
                    if (documentMembers.isEmpty()) {
                        writer.writeInline("[]")
                        return@openBlock
                    }

                    for ((i, member) in documentMembers.withIndex()) {
                        val mName = symbolProvider.toMemberName(member)
                        val msymbol = symbolProvider.toSymbol(member)
                        val jsonName =
                            member.getTrait(JsonNameTrait::class.java).map { it.value }
                                .orElse(mName)
                        if (i == 0) {
                            writer.writeInline("[ ")
                        } else {
                            writer.writeInline(", ")
                        }
                        if (msymbol == Http.HTTPDate || (
                                msymbol.name == "Maybe" && msymbol.references.map {
                                    it.symbol
                                }.contains(Http.HTTPDate)
                                )
                        ) {
                            if (msymbol.name == "Maybe") {
                                writer.write(
                                    "\"$jsonName\" #{aeson:N}..= ((#{textenc:N}.decodeUtf8 . #N.formatHTTPDate) #{functor:N}.<$> #{input:N}.$mName $varName)",
                                    Http.HTTPDate
                                )
                            } else {
                                writer.write(
                                    "\"$jsonName\" #{aeson:N}..= (#{textenc:N}.decodeUtf8 $ #N.formatHTTPDate $ #{input:N}.$mName $varName)",
                                    Http.HTTPDate
                                )
                            }
                        } else {
                            writer.write("\"$jsonName\" #{aeson:N}..= #{input:N}.$mName $varName")
                        }
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
                    "#{query:N}.renderQuery True (#{query:N}.queryTextToQuery m)"
                )
                writer.openBlock("where", "") {
                    if (mapParams != null) {
                        writer.openBlock("reservedParams =", "") {
                            if (preloadedParams.isEmpty()) {
                                writer.writeInline("[]")
                                return@openBlock
                            }
                            for ((i, param) in preloadedParams.withIndex()) {
                                if (i == 0) {
                                    writer.write("[ \"$param\"")
                                } else {
                                    writer.write(", \"$param\"")
                                }
                            }
                            writer.write("]")
                        }

                        val memberShape = mapParams.member
                        val memberName = symbolProvider.toMemberName(memberShape)
                        val target = model.expectShape(
                            memberShape?.target,
                            MapShape::class.java
                        )

                        writer.openBlock(
                            "mapParams = #{input:N}.$memberName input",
                            "",
                        ) {
                            writer.write("#{flip:T} #{maybe:N}.fromMaybe #{map:N}.empty")
                            writer.write("#{flip:T} #{map:N}.toList")
                            writer.write(
                                "#{flip:T} (#{list:N}.filter (\\(k, _) -> not $ #{list:N}.any (== k) reservedParams))"
                            )
                            if (target.value.isListShape) {
                                writer.write(
                                    "#{flip:T} (#{list:N}.concatMap (\\(k, v) -> #{list:N}.map (\\x -> (k, #{maybe:N}.Just x)) v))"
                                )
                            } else {
                                writer.write("#{flip:T} (#{list:N}.map (\\(k, v) -> (k, #{maybe:N}.Just v)))")
                            }
                        }
                    }

                    writer.openBlock("staticParams = []", "") {
                        for ((k, v) in literalParams) {
                            writer.write("++[(\"$k\", #{maybe:N}.Just \"$v\")]")
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
                            val isOfStringType = if (target.isListShape) {
                                val inner = model.expectShape(
                                    target.asListShape().get().member.target
                                )
                                inner.isStringShape
                            } else {
                                target.isStringShape
                            }
                            if (target.isListShape) {
                                val inner =
                                    if (isOfStringType) "x" else "#{text:N}.pack $ show x"
                                writer.write(
                                    "++ (#{list:N}.concatMap (\\x -> ($query, #{maybe:N}.Just ($inner))) (#{input:N}.$memberName input))"
                                )
                            } else {
                                val inner =
                                    if (isOfStringType) "#{input:N}.$memberName input" else "#{text:N}.pack $ show (#{input:N}.$memberName input)"
                                writer.write(
                                    "++ [($query, #{maybe:N}.Just ($inner))]"
                                )
                            }
                        }
                    }

                    if (mapParams != null) {
                        writer.write(
                            "m = staticParams ++ mapParams ++ dynamicParams"
                        )
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
        val uriSegments = httpTrait.uri.segments

        // Handle implement toString for each type

        writer.write("requestPath :: #{input:T} -> #{byteString:T}")
        writer.openBlock("requestPath input = ", "") {
            writer.write("#T _path", EncodingUtf8)
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
        val responseSymbol =
            symbolProvider.toSymbol(model.expectShape(opShape.outputShape))
        val errorSymbol = operationErrorSymbol(opSymbol)
        val responseType = responseSymbol.toEither(errorSymbol).inIO()
    }
}
