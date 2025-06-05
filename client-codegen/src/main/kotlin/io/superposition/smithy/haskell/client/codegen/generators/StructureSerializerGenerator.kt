package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.*
import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import software.amazon.smithy.model.shapes.StructureShape

@Suppress("MaxLineLength")
class StructureSerializerGenerator(
    private val directive: HaskellShapeDirective<StructureShape>,
    private val writer: HaskellWriter
) : Runnable {
    override fun run() {
        writer.pushState()
        writer.putContext("encoding", HaskellSymbol.EncodingUtf8)
        writer.putContext("hdate", Http.HTTPDate)
        writer.openBlock("instance #{aeson:N}.ToJSON #T where", "", directive.symbol()) {
            writer.openBlock("toJSON a = #{aeson:N}.object [", "") {
                val members = directive.shape().members().toList()
                writer.writeList(members) {
                    val sym = directive.symbolProvider().toSymbol(it)
                    val sb = StringBuilder()
                        .append("${it.jsonName.dq} #{aeson:N}..= ")
                    if (sym.isOrWrapped(Http.HTTPDate)) {
                        sb.append("((${it.memberName} a) ")
                        if (sym.isMaybe()) {
                            sb.append("#{functor:N}.<&> ")
                        } else {
                            sb.append("#{and:T} ")
                        }
                        sb.append("(#{encoding:N}.decodeUtf8 . #{hdate:N}.formatHTTPDate))")
                    } else {
                        sb.append("${it.memberName} a")
                    }
                    writer.format(sb.toString())
                }
                writer.write("]")
            }
        }
        writer.popState()
    }
}
