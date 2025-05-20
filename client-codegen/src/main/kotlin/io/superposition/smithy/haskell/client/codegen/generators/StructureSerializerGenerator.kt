package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellWriter
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
                for ((i, member) in members.withIndex()) {
                    val memberName = symbolProvider.toMemberName(member)
                    if (i == 0) {
                        writer.writeInline("[ ")
                    } else {
                        writer.writeInline(", ")
                    }

                    val jsonName = getJsonName(member)
                    writer.write("\"$jsonName\" .= $memberName a")
                }
                writer.write("]")
            }
        }
    }
}
