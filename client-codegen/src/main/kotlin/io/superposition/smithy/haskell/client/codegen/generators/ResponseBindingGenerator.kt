@file:Suppress("LongMethod")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.CodegenUtils
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.ByteStringChar8
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.ParseEither
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.Http
import io.superposition.smithy.haskell.client.codegen.isOrWrapped
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.traits.HttpHeaderTrait
import software.amazon.smithy.model.traits.HttpPrefixHeadersTrait
import software.amazon.smithy.model.traits.HttpTrait
import software.amazon.smithy.model.traits.JsonNameTrait

private typealias MemberToVariable = Pair<MemberShape, String>

@Suppress(
    "MaxLineLength",
    "ktlint:standard:max-line-length",
    "TooManyFunctions",
    "LargeClass"
)
class ResponseBindingGenerator(
    private val operation: OperationShape,
    private val model: Model,
    private val writer: HaskellWriter,
    private val symbolProvider: SymbolProvider,
) : Runnable {
    private val httpBindingIndex = HttpBindingIndex.of(model)
    private val httpTrait = operation.getTrait(HttpTrait::class.java).orElse(null)
    private val bindings = httpBindingIndex.getResponseBindings(operation)

    private val outputSymbol =
        symbolProvider.toSymbol(model.expectShape(operation.outputShape))

    private val getBindings = { location: HttpBinding.Location ->
        bindings.filter { it.value.location == location }.map { it.value }
    }

    override fun run() {
        writer.pushState()
        writer.putContext("parseEither", ParseEither)
        deserializeResponseFn()
        writer.popState()
    }

    private fun deserializeHeaderMembers(
        writer: HaskellWriter,
        vars: MutableList<MemberToVariable>
    ) {
        val headerBindings = getBindings(HttpBinding.Location.HEADER)
        val prefixHeaderBindings = getBindings(HttpBinding.Location.PREFIX_HEADERS)

        writer.putContext("httpDate", Http.HTTPDate)
        writer.putContext("bsChar8", ByteStringChar8)
        for (binding in headerBindings) {
            val member = binding.member
            val hName = (binding.bindingTrait.get() as HttpHeaderTrait).value

            val name = member.memberName
            val symbol = symbolProvider.toSymbol(member)

            vars.add(member to "${name}HeaderE")
            writer.openBlock(
                "${name}HeaderE :: #T <-",
                "",
                symbol
            ) {
                writer.write("findHeader ${hName.dq}")
                val decode = if (symbol.isOrWrapped(Http.HTTPDate)) {
                    "#{httpDate:N}.parseHTTPDate"
                } else {
                    "#{aeson:N}.decodeStrict"
                }
                writer.write("#{flip:T} #{maybe:N}.maybe #{nothing:T} ($decode)")

                if (member.isOptional) {
                    writer.write("#{flip:T} #{right:T}")
                } else {
                    writer.write(
                        "#{flip:T} #{maybe:N}.maybe (#{left:T} ${"$name not found in header".dq}) (#{right:T})"
                    )
                }
            }
        }

        for (binding in prefixHeaderBindings) {
            val member = binding.member
            val hPrefix = (binding.bindingTrait.get() as HttpPrefixHeadersTrait).value

            val name = member.memberName
            val symbol = symbolProvider.toSymbol(member)

            vars.add(member to "${name}HeaderE")
            writer.openBlock(
                "${name}HeaderE :: #T <- do",
                "",
                symbol
            ) {
                writer.write("filterHeaderByPrefix ${hPrefix.dq}")
                writer.write(
                    "#{flip:T} #{list:N}.map #C",
                    Runnable {
                        writer.write(
                            "(\\(n, v) -> (stripPrefix ${hPrefix.dq} n, #{encoding:N}.decodeUtf8 v))"
                        )
                    }
                )
                writer.write("#{flip:T} #{map:N}.fromList")
                if (member.isOptional) {
                    writer.write("#{flip:T} #{just:T}")
                }
                writer.write("#{flip:T} #{right:T}")
            }
        }
    }

    private fun deserializePayloadMembers(
        writer: HaskellWriter,
        vars: MutableList<MemberToVariable>
    ) {
        val payloadBinding = getBindings(HttpBinding.Location.PAYLOAD).firstOrNull()
        val documentBindings = getBindings(HttpBinding.Location.DOCUMENT)

        if (payloadBinding == null && documentBindings.isEmpty()) {
            return
        }

        if (payloadBinding != null) {
            val member = payloadBinding.member
            val name = member.memberName
            val symbol = symbolProvider.toSymbol(member)

            vars.add(member to "${name}PayloadE")
            writer.openBlock(
                "${name}PayloadE :: #T <- do",
                "",
                symbol
            ) {
                writer.write("#{aeson:N}.decode (#{httpClient:N}.responseBody response)")
                if (member.isOptional) {
                    writer.write("#{flip:T} #{right:T}")
                } else {
                    writer.write(
                        "#{flip:T} #{maybe:N}.maybe (#{left:T} ${"failed to parse response".dq}) (#{right:T})"
                    )
                }
            }
        } else {
            writer.openBlock(
                "responseObject :: #{aeson:N}.Object <-",
                ""
            ) {
                writer.write("#{httpClient:N}.responseBody response")
                writer.write("#{flip:T} #{aeson:N}.decode")
                writer.write(
                    "#{flip:T} #{maybe:N}.maybe (#{left:T} ${"failed to parse response body".dq}) (#{right:T})"
                )
            }

            for (binding in documentBindings) {
                val member = binding.member
                val name = member.memberName
                val jsonName = binding.member.getTrait(JsonNameTrait::class.java)
                    .map { it.value }.orElse(name)

                val symbol = symbolProvider.toSymbol(member)

                vars.add(member to "${name}DocumentE")
                writer.openBlock(
                    "${name}DocumentE :: #T <-",
                    "",
                    symbol
                ) {
                    val parser = if (member.isOptional) {
                        "#{aeson:N}..:?"
                    } else {
                        "#{aeson:N}..:"
                    }

                    writer.write("#{parseEither:T} (flip ($parser) ${jsonName.dq}) responseObject")
                    writer.openBlock("#{flip:T} \\case", "") {
                        writer.write(
                            "#{left:T} err -> #{left:T} (#{text:N}.pack err)"
                        )
                        writer.write(
                            "#{right:T} value -> #{right:T} value"
                        )
                    }
                }
            }
        }
    }

    private fun renderOutput(writer: HaskellWriter, vars: List<MemberToVariable>) {
        writer.openBlock(
            "#{output:N}.build $ do",
            "",
        ) {
            for ((member, variable) in vars) {
                val setter = CodegenUtils.getSetterName(member.memberName)
                writer.write("#{output:N}.$setter $variable")
            }
        }
    }

    private fun deserializeResponseFn() {
        val prefixHeaderBindings = getBindings(HttpBinding.Location.PREFIX_HEADERS)
        val payloadBinding = getBindings(HttpBinding.Location.PAYLOAD).firstOrNull()

        writer.write(
            "deserializeResponse :: #{httpClient:N}.Response #{lazyByteString:T} -> #{either:T} #{text:T} #{output:T}"
        )
        writer.openBlock(
            "deserializeResponse response = do",
            ""
        ) {
            val template = """
            #{headers:C|}
            #{payload:C|}
            #{result:C|}
            """.trimIndent()

            val vars = mutableListOf<MemberToVariable>()
            writer.pushState()
            writer.putContext(
                "headers",
                Runnable {
                    deserializeHeaderMembers(writer, vars)
                }
            )
            writer.putContext(
                "payload",
                Runnable {
                    deserializePayloadMembers(writer, vars)
                }
            )
            writer.putContext(
                "result",
                Runnable {
                    renderOutput(writer, vars)
                }
            )
            writer.write(template)
            writer.popState()

            writer.openBlock("where", "") {
                writer.openBlock(
                    "headers = #{httpClient:N}.responseHeaders response",
                    ""
                ) {
                    writer.write(
                        "#{flip:T} #{list:N}.map (\\(n, v) -> (#{encoding:N}.decodeUtf8 (#{ci:N}.original n), v))"
                    )
                }
                writer.write("findHeader name = snd #{functor:N}.<$> #{list:N}.find ((name ==) . fst) headers")
                if (prefixHeaderBindings.isNotEmpty()) {
                    writer.write(
                        "filterHeaderByPrefix prefix = #{list:N}.filter (#{text:N}.isPrefixOf prefix . fst) headers"
                    )
                    writer.write("stripPrefix prefix s = #{maybe:N}.maybe s (id) $ #{text:N}.stripPrefix prefix s")
                }
            }
        }
    }
}
