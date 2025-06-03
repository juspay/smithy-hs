@file:Suppress("LongMethod")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.comma
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.toHaskellHttpMethod
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.EncodingUtf8
import io.superposition.smithy.haskell.client.codegen.language.ClientRecord
import software.amazon.smithy.codegen.core.CodegenException
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.MapShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.traits.HttpHeaderTrait
import software.amazon.smithy.model.traits.HttpPrefixHeadersTrait
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
        writer.pushState()
        writer.write(
            """
            class RequestSegment a where
                toRequestSegment :: Show a => a -> #{text:T}
            instance RequestSegment #{text:T} where
                toRequestSegment = id
            instance RequestSegment Integer where
                toRequestSegment = #{text:N}.pack . show
            instance RequestSegment Bool where
                toRequestSegment = #{text:N}.toLower . #{text:N}.pack . show
            """.trimIndent()
        )
        writer.write("")

        writer.putContext("requestHeaders", HaskellSymbol.Http.RequestHeaders)
        val payloadGeneratorStatus = serializePayloadFn()
        val queryGeneratorStatus = serializeQueryFn()
        val headerGeneratorStatus = serializeHeadersFn()
        serializeLabelFn()
        operationFn(payloadGeneratorStatus, queryGeneratorStatus, headerGeneratorStatus)
        writer.popState()
    }

    private fun toStringSerializer(
        binding: HttpBinding,
        formatter: (String) -> String
    ): String {
        val name = binding.memberName
        val chainFn = if (binding.member.isOptional) HaskellSymbol.FlippedFmap else HaskellSymbol.And

        val chain = writer.newCallChain("(#{input:N}.$name input", chainFn)
            .chainIf("#{list:N}.map (toRequestSegment))", binding.member.isMemberListShape(model))
            .chainIf("toRequestSegment)", !binding.member.isMemberListShape(model))
            .toString()

        return formatter(chain)
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
                writer.putContext("encoding", EncodingUtf8)
                writer.putContext("hdate", Http.HTTPDate)
                writer.openBlock(
                    "#{httpClient:N}.RequestBodyLBS $ #{aeson:N}.encode $ #{aeson:N}.object [",
                    ""
                ) {
                    writer.writeList(documentBindings) { binding ->
                        val memberName = binding.memberName
                        val jsonName = binding.member.getTrait(JsonNameTrait::class.java)
                            .map { it.value }.orElse(memberName)
                        val member = binding.member
                        val sym = symbolProvider.toSymbol(member)
                        val sb = StringBuilder()
                            .append("${jsonName.dq} #{aeson:N}..= ")
                        if (sym.isOrWrapped(Http.HTTPDate)) {
                            sb.append("((#{input:N}.$memberName input) ")
                            if (sym.isMaybe()) {
                                sb.append("#{functor:N}.<&> ")
                            } else {
                                sb.append("#{and:T} ")
                            }
                            sb.append("(#{encoding:N}.decodeUtf8 . #{hdate:N}.formatHTTPDate))")
                        } else {
                            sb.append("#{input:N}.$memberName input")
                        }
                        writer.format(sb.toString())
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

        val listForEach = { fn: String, isList: Boolean ->
            if (isList) {
                "($fn . #{list:N}.concatMap (expandTuple))"
            } else {
                fn
            }
        }

        writer.write("$fnName :: #{input:T} -> #{byteString:T}")
        writer.openBlock("$fnName input =", "") {
            writer.openBlock("let", "") {
                val vars = mutableListOf("staticParams")

                writer.openBlock("staticParams = [", "") {
                    writer.writeList(literalParams.toList()) { (k, v): Pair<String, String> ->
                        writer.format("toQueryItem (${k.dq}, ${v.dq})")
                    }
                    writer.write("]")
                }

                if (mapBinding != null) {
                    vars.add("mapParams")
                    val memberShape = mapBinding.member
                    val memberName = symbolProvider.toMemberName(memberShape)
                    val valueMember = model.expectShape(
                        memberShape?.target,
                        MapShape::class.java
                    ).value

                    writer.newCallChain("mapParams = #{input:N}.$memberName input")
                        .chainIf("#{maybe:N}.maybe [] (#{map:N}.toList)", memberShape.isOptional)
                        .chainIf("#{map:N}.toList", !memberShape.isOptional)
                        .chain(
                            "(#{list:N}.filter (\\(k, _) -> not $ #{list:N}.any (== k) reservedParams))"
                        )
                        .chain(listForEach("toQuery", valueMember.isMemberListShape(model)))
                        .close()
                }

                for (param in queryBindings) {
                    val member = param.member
                    val name = symbolProvider.toMemberName(member)
                    val target = model.expectShape(
                        member?.target
                    )

                    val query = param.locationName.dq

                    val varName = "${name}Query"
                    val chainFn = if (member.isOptional) HaskellSymbol.FlippedFmap else HaskellSymbol.And
                    writer.newCallChain("$varName = #{input:N}.$name input", chainFn)
                        .chainIf("(\\x -> [x])", !member.isMemberListShape(model))
                        .chain("#{list:N}.map (toRequestSegment)")
                        .chain("#{list:N}.map (\\x -> toQueryItem ($query, x))")
                        .setChainFn(HaskellSymbol.And)
                        .chainIf("#{maybe:N}.maybe [] (id)", member.isOptional)
                        .close()
                    vars.add(varName)
                }

                writer.write("m = ${vars.joinToString(" ++ ")}")
                writer.write("in #{query:N}.renderQuery True (#{query:N}.queryTextToQuery m)")
            }
            writer.openBlock("where", "") {
                writer.write("toQueryItem (k, v) = (k, #{maybe:N}.Just v)")
                writer.write("toQuery = #{list:N}.map (toQueryItem)")
                writer.write("expandTuple (key, values) = #{list:N}.map (\\v -> (key, v)) values")
                if (mapBinding != null) {
                    writer.openBlock("reservedParams = [", "") {
                        writer.writeList(preloadedParams.toList()) { writer.format(it.dq) }
                        writer.write("]")
                    }
                }
            }
        }

        return true
    }

    private fun serializeLabelFn() {
        val labelBindings = bindings.filter { it.value.location == HttpBinding.Location.LABEL }
        val uriSegments = httpTrait.uri.segments
        val fnName = bindingSerializeFnName(
            operationSymbol.name,
            HttpBinding.Location.LABEL
        )

        writer.write("$fnName :: #{input:T} -> #{byteString:T}")
        writer.openBlock("$fnName input = ", "") {
            writer.openBlock(
                "#{byteString:N}.toStrict $ #{byteStringBuilder:N}.toLazyByteString $ #{path:N}.encodePathSegmentsRelative [",
                ""
            ) {
                writer.writeList(uriSegments) { segment ->
                    if (segment.isLiteral) {
                        writer.format(segment.content.dq)
                    } else if (segment.isLabel) {
                        val binding = labelBindings[segment.content]
                            ?: throw CodegenException("Label binding not found for: ${segment.content}")
                        toStringSerializer(binding) { s -> writer.format(s) }
                    } else {
                        throw CodegenException(
                            "Only literal and label segments supported in path: ${segment.content}"
                        )
                    }
                }
                writer.write("]")
            }
        }
    }

    private fun serializeHeadersFn(): Boolean {
        val fnName =
            bindingSerializeFnName(operationSymbol.name, HttpBinding.Location.HEADER)
        val headerBindings = getBindings(HttpBinding.Location.HEADER)
        val prefixHeaderBindings =
            getBindings(HttpBinding.Location.PREFIX_HEADERS)
        if (headerBindings.isEmpty() && prefixHeaderBindings.isEmpty()) {
            return false
        }

        // TODO handle non string labels
        writer.write("$fnName :: #{input:T} -> #{requestHeaders:T}")
        writer.openBlock("$fnName input =", "") {
            val vars = mutableListOf<String>()
            writer.openBlock("let ", "") {
                for (binding in headerBindings) {
                    val member = binding.member
                    val hName = (binding.bindingTrait.get() as HttpHeaderTrait).value
                    val name = member.memberName

                    val varName = "${name}Header"
                    val chainFn = if (member.isOptional) HaskellSymbol.FlippedFmap else HaskellSymbol.And

                    val chainHead =
                        toStringSerializer(binding) { s -> "$varName = $s" }
                    writer.newCallChain(chainHead, chainFn)
                        .chainIf("#{text:N}.intercalate $comma", member.isMemberListShape(model))
                        .chain("\\x -> [(${hName.dq}, #{encoding:N}.encodeUtf8 x)]")
                        .chainIf("#{just:T}", !member.isOptional)
                        .close()
                    vars.add(varName)
                }

                for (binding in prefixHeaderBindings) {
                    val member = binding.member
                    val hPrefix = (binding.bindingTrait.get() as HttpPrefixHeadersTrait).value
                    val name = member.memberName
                    val valueMember = model.expectShape(
                        member?.target,
                        MapShape::class.java
                    ).value

                    val chainFn = if (member.isOptional) HaskellSymbol.FlippedFmap else HaskellSymbol.And

                    val varName = "${name}Header"
                    writer.newCallChain("$varName = #{input:N}.$name input", chainFn)
                        .chain("#{map:N}.toList")
                        .chainIf(
                            "#{list:N}.map (\\(n, v) -> (n, #{text:N}.intercalate $comma v))",
                            valueMember.isMemberListShape(model)
                        )
                        .chain(
                            "#{list:N}.map (\\(n, v) -> (toHeaderName ${hPrefix.dq} n, #{encoding:N}.encodeUtf8 v))"
                        )
                        .chainIf("#{just:T}", !member.isOptional)
                        .close()
                    vars.add(varName)
                }

                writer.openBlock("in #{list:N}.concat $ #{maybe:N}.catMaybes [", "") {
                    writer.writeList(vars) { it }
                    writer.write("]")
                }
            }
            if (prefixHeaderBindings.isNotEmpty()) {
                writer.openBlock("where", "") {
                    writer.write("toHeaderName prefix name = #{ci:N}.mk $ #{encoding:N}.encodeUtf8 $ prefix <> name")
                }
            }
        }

        return true
    }

    private fun operationFn(
        payloadGeneratorStatus: Boolean,
        queryGeneratorStatus: Boolean,
        headerGeneratorStatus: Boolean
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
        val headerSerializeFn =
            bindingSerializeFnName(operationSymbol.name, HttpBinding.Location.HEADER)

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
                    writer.write("method = ${httpTrait.method.dq}")
                } else {
                    writer.write("method = #T", method)
                }
                writer.openBlock("toRequest input req =", "") {
                    writer.openBlock("req {", "}") {
                        writer.write("#T = $labelSerializeFn input", Http.rqPath)
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
                        if (headerGeneratorStatus) {
                            writer.write(
                                ", #T = $headerSerializeFn input",
                                Http.rqHeaders
                            )
                        }
                    }
                }
            }
        }
    }
}
