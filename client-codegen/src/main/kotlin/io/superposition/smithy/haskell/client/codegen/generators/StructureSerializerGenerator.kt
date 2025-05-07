package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.model.traits.JsonNameTrait

@Suppress("MaxLineLength")
class StructureSerializerGenerator(private val context: HaskellContext, private val shape: StructureShape) {
    private fun getJsonName(member: MemberShape): String {
        val jsonNameTrait = member.getTrait(JsonNameTrait::class.java)
        return if (jsonNameTrait.isPresent) {
            jsonNameTrait.get().value
        } else {
            member.memberName
        }
    }

    private fun renderMember(writer: HaskellWriter, member: MemberShape, memberName: String, first: Boolean) {
        if (first) {
            writer.writeInline("[ ")
        } else {
            writer.writeInline(", ")
        }

        val jsonName = getJsonName(member)
        writer.write("\"$jsonName\" .= $memberName a")
    }

    fun generate() {
        val symbol = context.symbolProvider().toSymbol(shape)
        println("SERDE:: Members of ${symbol.name}")
        for (member in shape.members()) {
            println("${member.memberName} is of type ${member.type} with target ${member.target.name}")

            val target = context.model.expectShape(member.target)
            when (target) {
                is StructureShape -> {
                    println("Can be casted to structure")
                    StructureSerializerGenerator(context, target.asStructureShape().get()).generate()
                }
            }
            // if (member.isStructureShape) {
            // }
            // else if (member.isListShape) {
            // } else if (member.isUnionShape) {
            // } else if (member.isEnumShape) {
            // } else {
            // }
        }

        context.writerDelegator().useShapeWriter(shape) { writer ->
            writer.write("-- ToJSON implementation for ${symbol.name}")
            writer.openBlock("instance ToJSON ${symbol.name}", "") {
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
