@file:Suppress("LongMethod")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.Char8
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.ParseEither
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.knowledge.HttpBindingIndex
import software.amazon.smithy.model.shapes.ListShape
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.shapes.StringShape
import software.amazon.smithy.model.shapes.TimestampShape
import software.amazon.smithy.model.traits.HttpHeaderTrait
import software.amazon.smithy.model.traits.HttpPrefixHeadersTrait

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
    private val bindings = httpBindingIndex.getResponseBindings(operation)

    private val getBindings = { location: HttpBinding.Location ->
        bindings.filter { it.value.location == location }.map { it.value }
    }

    override fun run() {
        writer.pushState()
        writer.putContext("parseEither", ParseEither)
        writer.putContext("httpDate", Http.HTTPDate)
        writer.putContext("char8", Char8)
        deserializeResponseFn()
        writer.popState()
    }

    private fun deserializeHeaderMembers(
        writer: HaskellWriter,
        vars: MutableList<MemberToVariable>
    ) {
        val headerBindings = getBindings(HttpBinding.Location.HEADER)
        val prefixHeaderBindings = getBindings(HttpBinding.Location.PREFIX_HEADERS)

        for (binding in headerBindings) {
            val member = binding.member
            val hName = (binding.bindingTrait.get() as HttpHeaderTrait).value

            val name = member.fieldName
            val symbol = symbolProvider.toSymbol(member)

            val innerType = if (member.isMemberListShape(model)) {
                model.expectShape(model.expectShape(member.target, ListShape::class.java).member.target)
            } else {
                model.expectShape(member.target)
            }

            vars.add(member to "${name}HeaderE")
            writer.openBlock(
                "${name}HeaderE :: #T <-",
                "",
                symbol
            ) {
                var parser = when (innerType) {
                    is StringShape -> "parseTextHeader"
                    is TimestampShape -> "parseTimestampHeader"
                    else -> "parseHeader"
                }
                if (member.isMemberListShape(model)) {
                    parser = "parseHeaderList $parser"
                }

                writer.newCallChain("(findHeader ${hName.dq} #{functor:N}.<&> $parser)")
                    .chainIf("sequence", member.isOptional)
                    .chainIf(
                        "#{maybe:N}.maybe (#{left:T} ${"$name not found in header".dq}) (id)",
                        !member.isOptional
                    )
                    .close()
            }
        }

        for (binding in prefixHeaderBindings) {
            val member = binding.member
            val hPrefix = (binding.bindingTrait.get() as HttpPrefixHeadersTrait).value

            val name = member.fieldName
            val symbol = symbolProvider.toSymbol(member)

            vars.add(member to "${name}HeaderE")
            writer.openBlock(
                "${name}HeaderE :: #T <- do",
                "",
                symbol
            ) {
                writer.newCallChain("filterHeaderByPrefix ${hPrefix.dq}")
                    .chain(
                        "#{list:N}.map #C",
                        Runnable {
                            writer.write(
                                "(\\(n, v) -> (stripPrefix ${hPrefix.dq} n, #{encoding:N}.decodeUtf8 v))"
                            )
                        }
                    )
                    .chain("#{map:N}.fromList")
                    .chainIf("#{just:T}", member.isOptional)
                    .chain("#{right:T}")
                    .close()
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
            val name = member.fieldName
            val symbol = symbolProvider.toSymbol(member)

            vars.add(member to "${name}PayloadE")
            writer.openBlock(
                "${name}PayloadE :: #T <- do",
                "",
                symbol
            ) {
                writer.newCallChain("#{aeson:N}.decode (#{httpClient:N}.responseBody response)")
                    .chainIf("#{right:T}", member.isOptional)
                    .chainIf(
                        "#{maybe:N}.maybe (#{left:T} ${"$name not found in payload".dq}) (#{right:T})",
                        !member.isOptional
                    )
                    .close()
            }
        } else {
            writer.openBlock(
                "responseObject :: #{aeson:N}.Object <-",
                ""
            ) {
                writer.newCallChain("#{httpClient:N}.responseBody response")
                    .chain("#{aeson:N}.decode")
                    .chain(
                        "#{maybe:N}.maybe (#{left:T} ${"failed to parse response body".dq}) (#{right:T})"
                    )
                    .close()
            }

            for (binding in documentBindings) {
                val member = binding.member
                val name = member.fieldName
                val jsonName = binding.jsonName

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
        val outputShape = model.expectShape(operation.outputShape)
        writer.openBlock(
            "#{output:N}.build $ do",
            "",
        ) {
            if (outputShape.members().isEmpty()) {
                writer.write("pure ()")
            }
            for ((member, variable) in vars) {
                val setter = CodegenUtils.getSetterName(member.fieldName)
                writer.write("#{output:N}.$setter $variable")
            }
        }
    }

    private fun deserializeResponseFn() {
        val prefixHeaderBindings = getBindings(HttpBinding.Location.PREFIX_HEADERS)

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
                writer.newCallChain("headers = #{httpClient:N}.responseHeaders response")
                    .chain("#{list:N}.map (\\(n, v) -> (#{encoding:N}.decodeUtf8 (#{ci:N}.original n), v))")
                    .close()

                writer.write("findHeader name = snd #{functor:N}.<$> #{list:N}.find ((name ==) . fst) headers")

                writer.write("parseTextHeader :: #{byteString:T} -> #{either:T} #{text:T} #{text:T}")
                writer.openBlock("parseTextHeader v = #{encoding:N}.decodeUtf8' v #{and:T} \\ case", "") {
                    writer.write(
                        "#{left:T} err -> #{left:T} $ #{text:N}.pack $ show err"
                    )
                    writer.write(
                        "#{right:T} value -> #{right:T} value"
                    )
                }

                writer.write(
                    "parseTimestampHeader :: #{aeson:N}.FromJSON a => #{byteString:T} -> #{either:T} #{text:T} a"
                )
                writer.write(
                    "parseTimestampHeader v = #{utility:N}.mapLeft (#{text:N}.pack) $ #{aeson:N}.eitherDecodeStrict' v"
                )

                writer.write("parseHeader :: #{aeson:N}.FromJSON a => #{byteString:T} -> #{either:T} #{text:T} a")
                writer.openBlock("parseHeader v = #{aeson:N}.eitherDecodeStrict v #{and:T} \\ case", "") {
                    writer.write(
                        "#{left:T} err -> #{left:T} $ #{text:N}.pack $ show err"
                    )
                    writer.write(
                        "#{right:T} value -> #{right:T} value"
                    )
                }

                writer.write(
                    "parseHeaderList :: #{aeson:N}.FromJSON a => (#{byteString:T} -> #{either:T} #{text:T} a) -> #{byteString:T} -> #{either:T} #{text:T} [a]"
                )
                writer.write(
                    "parseHeaderList parser = sequence . #{list:N}.map (parser) . #{char8:N}.split ','"
                )
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
