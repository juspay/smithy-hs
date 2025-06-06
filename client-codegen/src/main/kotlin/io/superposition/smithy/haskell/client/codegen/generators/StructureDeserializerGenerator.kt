package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import software.amazon.smithy.model.shapes.StructureShape

class StructureDeserializerGenerator(
    private val directive: HaskellShapeDirective<StructureShape>,
    private val writer: HaskellWriter,
) : Runnable {
    override fun run() {
        val symbol = directive.symbol()
        val members = directive.shape().members()
        val structName = symbol.name
        writer.putContext("encoding", HaskellSymbol.EncodingUtf8)
        writer.putContext("hdate", Http.HTTPDate)
        writer.openBlock("instance #{aeson:N}.FromJSON $structName where", "") {
            if (members.isEmpty()) {
                writer.write("parseJSON = #{aeson:N}.withObject ${structName.dq} $ \\_ -> pure $ $structName")
                return@openBlock
            }
            writer.openBlock(
                "parseJSON = #{aeson:N}.withObject ${structName.dq} $ \\v -> $structName",
                ""
            ) {
                for ((i, member) in members.withIndex()) {
                    if (i == 0) {
                        writer.writeInline("#{functor:N}.<$> ")
                    } else {
                        writer.writeInline("#{applicative:N}.<*> ")
                    }
                    val ms = directive.symbolProvider().toSymbol(member)
                    val isHDate = ms.isOrWrapped(Http.HTTPDate)
                    writer.writeInline("(v #{aeson:N}..: ${member.jsonName.dq}")
                    if (isHDate) {
                        val err = "Failed to parse $symbol.$ms as ${Http.HTTPDate}"
                        val chainFn = if (ms.isMaybe()) {
                            HaskellSymbol.FFmap
                        } else {
                            HaskellSymbol.And
                        }
                        writer.write("")
                        writer.indent()
                        val cc = writer.newCallChain(" >>= \\t -> t", chainFn)
                            .chain("#{encoding:N}.encodeUtf8")
                            .chain("#{hdate:N}.parseHTTPDate")
                            .chain("#{maybe:N}.maybe (fail ${err.dq}) pure")
                        if (ms.isMaybe()) {
                            cc.setChainFn(HaskellSymbol.And)
                                .chain("#{maybe:N}.maybe (pure #{nothing:T}) pure")
                        }
                        cc.close()
                        writer.write(")")
                        writer.dedent()
                    } else {
                        writer.writeInline(")")
                        writer.write("")
                    }
                }
            }
        }
    }
}
