package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.CodegenUtils.dq
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

class FromResponseParserGenerator(
    private val shape: Shape,
    symbolProvider: SymbolProvider,
    private val context: HaskellContext,
    private val status: Int,
    private val httpBindings: Map<String, HttpBinding>,
) {
    private val symbol = symbolProvider.toSymbol(shape)
    private val name = symbol.name
    private val localVarContext = HashMap<String, String>()
    private val fieldMap =
        shape.members().toList().associate { it.memberName to it.fieldName }

    fun run() {
        context.writerDelegator.useShapeWriter(shape) { writer ->
            writer.pushState()
            writer.putContext("utility", context.utilitySymbol)
            writer.putContext("deSerHeaders", Runnable { deSerHeaders(writer) })
            writer.putContext("deSerPayload", Runnable { deSerPayload(writer) })
            writer.putContext("setFields", Runnable { setFields(writer) })
            writer.write(
                """
                instance #{utility:N}.FromResponseParser $name where
                    expectedStatus = #{httpTypes:N}.status$status
                    responseParser = do
                        #{deSerHeaders:C|}
                        #{deSerPayload:C|}
                        pure $ $name {
                            #{setFields:C|}
                        }
                """.trimIndent(),
            )
            writer.popState()
        }
    }

    private fun bindLocalVar(memberName: String): String {
        val varName = "var${localVarContext.size}"
        localVarContext[memberName] = varName
        return varName
    }

    private fun getBindings(location: Location): Map<String, HttpBinding> =
        httpBindings.filter {
            it.value.location ==
                location
        }

    private fun deSerHeaders(writer: HaskellWriter) {
        getBindings(Location.PREFIX_HEADERS).forEach { (memberName, binding) ->
            val prefix = (binding.bindingTrait.get() as HttpPrefixHeadersTrait).value.dq
            writer.write(
                "${bindLocalVar(memberName)} <- #{utility:N}.deSerHeaderMap $prefix",
            )
        }
        getBindings(Location.HEADER).forEach { (memberName, binding) ->
            val header = (binding.bindingTrait.get() as HttpHeaderTrait).value.dq
            writer.write(
                "${bindLocalVar(memberName)} <- #{utility:N}.deSerHeader $header",
            )
        }
    }

    private fun deSerPayload(writer: HaskellWriter) {
        val payloadBinding = getBindings(Location.PAYLOAD)
        val documentBindings = getBindings(Location.DOCUMENT)
        if (payloadBinding.isNotEmpty()) {
            assert(payloadBinding.size == 1 && documentBindings.isEmpty()) // OK
            val (memberName, _) = payloadBinding.entries.first()
            writer.write("${bindLocalVar(memberName)} <- #{utility:N}.deSerBody")
        } else {
            documentBindings.forEach { (memberName, binding) ->
                val jsonName = binding.jsonName.dq
                writer.write(
                    "${bindLocalVar(memberName)} <- #{utility:N}.deSerField $jsonName",
                )
            }
        }
    }

    private fun setFields(writer: HaskellWriter) {
        writer.writeList(shape.members().toList()) { memberShape ->
            val name = memberShape.memberName
            writer.format("${fieldMap[name]} = ${localVarContext[name]}")
        }
    }
}
