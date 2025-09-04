package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.CodegenUtils.dq
import `in`.juspay.smithy.haskell.client.codegen.CodegenUtils.toHaskellHttpMethod
import `in`.juspay.smithy.haskell.client.codegen.HaskellContext
import `in`.juspay.smithy.haskell.client.codegen.HaskellWriter
import `in`.juspay.smithy.haskell.client.codegen.fieldName
import `in`.juspay.smithy.haskell.client.codegen.jsonName
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBinding.Location
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.traits.HttpHeaderTrait
import software.amazon.smithy.model.traits.HttpPrefixHeadersTrait
import software.amazon.smithy.model.traits.HttpQueryTrait
import software.amazon.smithy.model.traits.HttpTrait

// Assumes that the symbol has already been written.
class IntoRequestBuilderGenerator(
    private val shape: Shape,
    symbolProvider: SymbolProvider,
    private val context: HaskellContext,
    private val httpTrait: HttpTrait,
    private val contentType: String?,
    private val httpBindings: Map<String, HttpBinding>
) {
    private val symbol = symbolProvider.toSymbol(shape)
    private val name = symbol.name
    private val methodSymbol = toHaskellHttpMethod(httpTrait.method)
    private val fieldMap =
        shape.members().toList().associate { it.memberName to it.fieldName }

    fun run() {
        context.writerDelegator.useShapeWriter(shape) { writer ->
            writer.pushState()
            writer.putContext("utility", context.utilitySymbol)
            writer.putContext("httpMethod", methodSymbol)
            writer.putContext("setPath", Runnable { setPath(writer) })
            writer.putContext("setQuery", Runnable { setQuery(writer) })
            writer.putContext("setHeaders", Runnable { setHeaders(writer) })
            writer.putContext("setPayload", Runnable { setPayload(writer) })
            writer.write(
                """
                instance #{utility:N}.IntoRequestBuilder $name where
                    intoRequestBuilder self = do
                        #{utility:N}.setMethod #{httpMethod:T}
                        #{setPath:C|}
                        #{setQuery:C|}
                        #{setHeaders:C|}
                        #{setPayload:C|}
                """.trimIndent()
            )
            writer.popState()
        }
    }

    private fun getBindings(location: Location): Map<String, HttpBinding> =
        httpBindings.filter { it.value.location == location }

    private fun setPath(writer: HaskellWriter) {
        writer.openBlock("#{utility:N}.setPath [", "    ]") {
            val uriSegments = httpTrait.uri.segments
            writer.writeList(uriSegments) { useg ->
                if (useg.isLiteral) {
                    writer.format(useg.content.dq)
                } else {
                    writer.format("#{utility:N}.serializeElement (${fieldMap[useg.content]} self)")
                }
            }
        }
    }

    private fun setQuery(writer: HaskellWriter) {
        // Query literals are lower in priority order than input.
        httpTrait.uri.queryLiterals.forEach {
            writer.write("#{utility:N}.serQuery ${it.key.dq} (${it.value.dq} :: #{text:T})")
        }
        getBindings(Location.QUERY_PARAMS).forEach { (memberName, _) ->
            writer.write("#{utility:N}.serQueryMap (${fieldMap[memberName]} self)")
        }
        getBindings(Location.QUERY).forEach { (memberName, binding) ->
            val queryName = (binding.bindingTrait.get() as HttpQueryTrait).value.dq
            writer.write("#{utility:N}.serQuery $queryName (${fieldMap[memberName]} self)")
        }
    }

    private fun setHeaders(writer: HaskellWriter) {
        getBindings(Location.PREFIX_HEADERS).forEach { (memberName, binding) ->
            val prefix = (binding.bindingTrait.get() as HttpPrefixHeadersTrait).value.dq
            writer.write("#{utility:N}.serHeaderMap $prefix (${fieldMap[memberName]} self)")
        }
        getBindings(Location.HEADER).forEach { (memberName, binding) ->
            val header = (binding.bindingTrait.get() as HttpHeaderTrait).value.dq
            writer.write("#{utility:N}.serHeader $header (${fieldMap[memberName]} self)")
        }
    }

    private fun setPayload(writer: HaskellWriter) {
        val payloadBinding = getBindings(Location.PAYLOAD)
        val documentBindings = getBindings(Location.DOCUMENT)
        if (payloadBinding.isNotEmpty()) {
            assert(payloadBinding.size == 1 && documentBindings.isEmpty()) // OK
            val (memberName, _) = payloadBinding.entries.first()
            writer.write("#{utility:N}.serBody ${contentType!!.dq} (${fieldMap[memberName]} self)")
        } else {
            documentBindings.forEach { (memberName, binding) ->
                val fieldName = binding.jsonName.dq
                writer.write("#{utility:N}.serField $fieldName (${fieldMap[memberName]} self)")
            }
        }
    }
}
