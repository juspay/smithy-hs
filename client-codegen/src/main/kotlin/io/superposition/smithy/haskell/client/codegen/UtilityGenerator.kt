package io.superposition.smithy.haskell.client.codegen

import io.superposition.smithy.haskell.client.codegen.CodegenUtils.dq
import software.amazon.smithy.codegen.core.directed.CustomizeDirective

class UtilityGenerator(
    private val directive: CustomizeDirective<HaskellContext, HaskellSettings>
) : Runnable {
    override fun run() {
        directive.context().writerDelegator().useSymbolWriter(directive.context().utilitySymbol) { writer ->
            writer.pushState()
            writer.putContext("httpDate", Http.HTTPDate)
            writer.putContext("encoding", HaskellSymbol.EncodingUtf8)

            writer.openBlock("instance #{aeson:N}.ToJSON #{httpDate:T} where", "") {
                writer.write(
                    "toJSON = #{aeson:N}.String . #{encoding:N}.decodeUtf8 . #{httpDate:N}.formatHTTPDate"
                )
            }

            writer.openBlock("instance #{aeson:N}.FromJSON #{httpDate:T} where", "") {
                writer.openBlock("parseJSON = #{aeson:N}.withText ${Http.HTTPDate.fullName.dq} $ \\t -> ", "") {
                    writer.newCallChain("#{httpDate:N}.parseHTTPDate (#{encoding:N}.encodeUtf8 t)")
                        .chain("#{maybe:N}.maybe (fail ${"Failed to parse HTTP date".dq}) pure")
                        .close()
                }
            }

            writer.popState()
        }
    }
}
