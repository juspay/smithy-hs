package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSymbol.FromJSON
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.model.traits.JsonNameTrait

@Suppress("MaxLineLength")
class StructureSerializerGenerator(
    private val context: HaskellContext,
    private val shape: StructureShape
) : Runnable {
    private fun getJsonName(member: MemberShape): String {
        val jsonNameTrait = member.getTrait(JsonNameTrait::class.java)
        return if (jsonNameTrait.isPresent) {
            jsonNameTrait.get().value
        } else {
            member.memberName
        }
    }

    private fun renderMember(
        writer: HaskellWriter,
        member: MemberShape,
        memberName: String,
        first: Boolean
    ) {
        if (first) {
            writer.writeInline("[ ")
        } else {
            writer.writeInline(", ")
        }

        val jsonName = getJsonName(member)
        writer.write("\"$jsonName\" .= $memberName a")
    }

    override fun run() {
        context.writerDelegator().useShapeWriter(shape) { writer ->
            val symbol = context.symbolProvider().toSymbol(shape)
            writer.write("-- ToJSON implementation for ${symbol.name}")
            writer.openBlock("instance #T ${symbol.name}", "", FromJSON) {
                writer.openBlock("toJSON a = object", "") {
                    for ((i, member) in shape.members().withIndex()) {
                        val memberName = context.symbolProvider().toMemberName(member)

                        renderMember(writer, member, memberName, i == 0)
                    }
                    writer.write("]")
                }
            }
        }
    }
}
