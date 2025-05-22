package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.Http
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.traits.JsonNameTrait

@Suppress("MaxLineLength")
class StructureSerializerGenerator(
    private val members: Collection<MemberShape>,
    private val symbol: Symbol,
    private val symbolProvider: SymbolProvider,
    private val writer: HaskellWriter,
) : Runnable {
    private fun getJsonName(member: MemberShape): String {
        val jsonNameTrait = member.getTrait(JsonNameTrait::class.java)
        return if (jsonNameTrait.isPresent) {
            jsonNameTrait.get().value
        } else {
            member.memberName
        }
    }

    override fun run() {
        writer.openBlock("instance #{aeson:N}.ToJSON #T where", "", symbol) {
            writer.openBlock("toJSON a = #{aeson:N}.object", "") {
                if (members.isEmpty()) {
                    writer.writeInline("[]")
                    return@openBlock
                }
                for ((i, member) in members.withIndex()) {
                    val msymbol = symbolProvider.toSymbol(member)
                    val memberName = symbolProvider.toMemberName(member)
                    if (i == 0) {
                        writer.writeInline("[ ")
                    } else {
                        writer.writeInline(", ")
                    }

                    val jsonName = getJsonName(member)
                    if (msymbol == Http.HTTPDate || (
                            msymbol.name == "Maybe" && msymbol.references.map {
                                it.symbol
                            }.contains(Http.HTTPDate)
                            )
                    ) {
                        if (msymbol.name == "Maybe") {
                            writer.write(
                                "\"$jsonName\" #{aeson:N}..= ((#{textenc:N}.decodeUtf8 . #N.formatHTTPDate) #{functor:N}.<$> $memberName a)",
                                Http.HTTPDate
                            )
                        } else {
                            writer.write(
                                "\"$jsonName\" #{aeson:N}..= (#{textenc:N}.decodeUtf8 $ #N.formatHTTPDate $ $memberName a)",
                                Http.HTTPDate
                            )
                        }
                    } else {
                        writer.write("\"$jsonName\" #{aeson:N}..= $memberName a")
                    }
                }
                writer.write("]")
            }
        }
    }
}
