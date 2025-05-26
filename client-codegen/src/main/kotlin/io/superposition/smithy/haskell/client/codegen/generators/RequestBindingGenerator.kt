@file:Suppress("LongMethod")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.toHaskellHttpMethod
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.EncodingUtf8
import io.superposition.smithy.haskell.client.codegen.language.ClientRecord
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.MapShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.traits.HttpTrait
import software.amazon.smithy.model.traits.JsonNameTrait

private fun bindingSerializeFnName(
    operationName: String,
    location: HttpBinding.Location,
): String {
    return "ser${operationName}${location.name.replaceFirstChar { it.uppercase() }}"
}

@Suppress(
    "MaxLineLength",
    "ktlint:standard:max-line-length",
    "TooManyFunctions",
    "LargeClass",
    "LargeMethods"
)
class RequestBindingGenerator(
    private val operation: OperationShape,
    private val model: Model,
    private val writer: HaskellWriter,
    private val symbolProvider: SymbolProvider,
    private val client: ClientRecord,
) : Runnable {
    private val httpBindingIndex = HttpBindingIndex.of(model)
    private val httpTrait = operation.getTrait(HttpTrait::class.java).orElse(null)

    private val operationSymbol = symbolProvider.toSymbol(operation)
    private val bindings = httpBindingIndex.getRequestBindings(operation)
    private val inputSymbol =
        symbolProvider.toSymbol(model.expectShape(operation.inputShape))
    private val outputSymbol =
        symbolProvider.toSymbol(model.expectShape(operation.outputShape))

    private val getBindings = { location: HttpBinding.Location ->
        bindings.filter { it.value.location == location }.map { it.value }
    }

    override fun run() {
        val payloadGeneratorStatus = serializePayloadFn()
        val queryGeneratorStatus = serializeQueryFn()
        serializeHeadersFn()
        serializeLabelFn()
        operationFn(payloadGeneratorStatus, queryGeneratorStatus)
    }

    private fun serializePayloadFn(): Boolean {
        val fnName =
            bindingSerializeFnName(operationSymbol.name, HttpBinding.Location.PAYLOAD)

        val payloadBinding = getBindings(HttpBinding.Location.PAYLOAD).firstOrNull()
        val documentBindings = getBindings(HttpBinding.Location.DOCUMENT)

        if (payloadBinding == null && documentBindings.isEmpty()) {
            return false
        }

        writer.write("$fnName:: #{input:T} -> #{httpClient:N}.RequestBody")
        writer.openBlock("$fnName input =", "") {
            if (payloadBinding != null) {
                writer.write("#{httpClient:N}.RequestBodyLBS $")
                writer.write("#{aeson:N}.encode $")
                writer.write("#{input:N}.${payloadBinding.memberName} input")
            } else {
                writer.openBlock(
                    "#{httpClient:N}.RequestBodyLBS $ #{aeson:N}.encode $ #{aeson:N}.object [",
                    ""
                ) {
                    writer.writeList(documentBindings) { binding ->
                        val memberName = binding.memberName
                        val jsonName = binding.member.getTrait(JsonNameTrait::class.java)
                            .map { it.value }.orElse(memberName)

                        writer.format("\"$jsonName\" #{aeson:N}..= #{input:N}.$memberName input")
                    }
                    writer.write("]")
                }
            }
        }
        return true
    }

    private fun serializeQueryFn(): Boolean {
        val fnName =
            bindingSerializeFnName(operationSymbol.name, HttpBinding.Location.QUERY)
        val queryBindings = getBindings(HttpBinding.Location.QUERY)
        val mapBinding = getBindings(HttpBinding.Location.QUERY_PARAMS).firstOrNull()
        val literalParams = httpTrait.uri.queryLiterals

        val preloadedParams = literalParams.keys + queryBindings.map { it.locationName }

        if (literalParams.isEmpty() && mapBinding == null && queryBindings.isEmpty()) {
            return false
        }

        writer.write("$fnName :: #{input:T} -> #{byteString:T}")
        writer.openBlock("$fnName input =", "") {
            writer.write(
                "#{query:N}.renderQuery True (#{query:N}.queryTextToQuery m)"
            )
            writer.openBlock("where", "") {
                if (mapBinding != null) {
                    writer.openBlock("reservedParams = [", "") {
                        writer.writeList(preloadedParams.toList()) { writer.format(it.dq()) }
                        writer.write("]")
                    }

                    val memberShape = mapBinding.member
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

                writer.openBlock("staticParams = [", "") {
                    writer.writeList(literalParams.toList()) { (k, v): Pair<String, String> ->
                        writer.format("(${k.dq()}, #{maybe:N}.Just ${v.dq()})")
                    }
                    writer.write("]")
                }

                writer.openBlock("dynamicParams = []", "") {
                    for (param in queryBindings) {
                        val memberShape = param.member
                        val memberName = symbolProvider.toMemberName(memberShape)
                        val target = model.expectShape(
                            memberShape?.target
                        )

                        val query = param.locationName.dq()
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
                                "++ [(#{list:N}.concatMap (\\x -> ($query, #{maybe:N}.Just ($inner))) (#{input:N}.$memberName input))]"
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

                if (mapBinding != null) {
                    writer.write(
                        "m = staticParams ++ mapParams ++ dynamicParams"
                    )
                } else {
                    writer.write("m = staticParams ++ dynamicParams")
                }
            }
        }

        return true
    }

    private fun serializeLabelFn() {
        // val labelBindings = requestBindings.filter { it.value.location == HttpBinding.Location.LABEL }
        //     .map { it.value }
        val uriSegments = httpTrait.uri.segments
        val pathSep = "/"
        val fnName = bindingSerializeFnName(
            operationSymbol.name,
            HttpBinding.Location.LABEL
        )

        // Handle implement toString for each type
        writer.write("$fnName :: #{input:T} -> #{byteString:T}")
        writer.openBlock("$fnName input = ", "") {
            writer.write("#T _path", EncodingUtf8)
            writer.openBlock("where", "") {
                writer.openBlock("_path = #{text:N}.empty", "") {
                    for (segment in uriSegments) {
                        if (segment.isLabel) {
                            val name = segment.content
                            writer.write("<> ${pathSep.dq()} <> (#{input:N}.$name input)")
                        } else {
                            writer.write("<> ${"$pathSep${segment.content}".dq()}")
                        }
                    }
                }
            }
        }
    }

    private fun operationFn(
        payloadGeneratorStatus: Boolean,
        queryGeneratorStatus: Boolean,
    ) {
        val operationInput =
            inputSymbol.toBuilder().name("${inputSymbol.name}Builder")
                .build()
        val operationOutput =
            operationSymbol.toBuilder().name("${operationSymbol.name}Error")
                .build()
                .toEither(outputSymbol)
                .inIO()

        val functionName = operationSymbol.name.replaceFirstChar { it.lowercase() }

        val payloadSerializeFn =
            bindingSerializeFnName(operationSymbol.name, HttpBinding.Location.PAYLOAD)
        val querySerializeFn =
            bindingSerializeFnName(operationSymbol.name, HttpBinding.Location.QUERY)
        val labelSerializeFn =
            bindingSerializeFnName(operationSymbol.name, HttpBinding.Location.LABEL)

        writer.write(
            "$functionName :: #{client:T} -> #T () -> #T",
            operationInput,
            operationOutput
        )
        writer.openBlock("$functionName client inputB = do", "") {
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
                    writer.write("return $ #{first:T} (RequestError) $ deserializeResponse response")
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
                        "let path = (#T req) <> ($labelSerializeFn input)",
                        "",
                        Http.rqPath
                    ) {
                        writer.openBlock("in req {", "}") {
                            writer.write("#T = path", Http.rqPath)
                            writer.write(", #T = method", Http.rqMethod)
                            if (queryGeneratorStatus) {
                                writer.write(
                                    ", #T = $querySerializeFn input",
                                    Http.rqQueryString
                                )
                            }
                            if (payloadGeneratorStatus) {
                                writer.write(
                                    ", #T = $payloadSerializeFn input",
                                    Http.rqBody
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    private fun serializeHeadersFn(): Boolean {
        return true
    }
}
