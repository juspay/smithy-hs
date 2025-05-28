package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.jsonName
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.model.shapes.MemberShape

class StructureSerializerGenerator(
    private val members: Collection<MemberShape>,
    private val symbol: Symbol,
    private val writer: HaskellWriter,
) : Runnable {
    override fun run() {
        writer.openBlock("instance #{aeson:N}.ToJSON #T where", "", symbol) {
            writer.openBlock("toJSON a = #{aeson:N}.object", "") {
                if (members.isEmpty()) {
                    writer.writeInline("[]")
                    return@openBlock
                }
                for ((i, member) in members.withIndex()) {
                    if (i == 0) {
                        writer.writeInline("[ ")
                    } else {
                        writer.writeInline(", ")
                    }

                    writer.write("${member.jsonName.dq} #{aeson:N}..= ${member.memberName} a")
                }
                writer.write("]")
            }
        }
    }
}
