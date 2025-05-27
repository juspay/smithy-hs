package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.jsonName
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.model.shapes.MemberShape

class StructureDeserializerGenerator(
    private val members: Collection<MemberShape>,
    private val symbol: Symbol,
    private val writer: HaskellWriter,
) : Runnable {
    override fun run() {
        val structName = symbol.name
        writer.openBlock("instance #{aeson:N}.FromJSON $structName where", "") {
            if (members.isEmpty()) {
                writer.write("parseJSON = #{aeson:N}.withObject ${structName.dq()} $ \\_ -> pure $ $structName")
                return@openBlock
            }
            writer.openBlock("parseJSON = #{aeson:N}.withObject ${structName.dq()} $ \\v -> $structName", "") {
                for ((i, member) in members.withIndex()) {
                    if (i == 0) {
                        writer.writeInline("#{functor:N}.<$> ")
                    } else {
                        writer.writeInline("#{applicative:N}.<*> ")
                    }
                    writer.write("v #{aeson:N}..: ${member.jsonName().dq()}")
                }
            }
        }
    }
}
