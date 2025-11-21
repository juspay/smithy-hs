@file:Suppress("MaxLineLength")

package `in`.juspay.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.directed.CustomizeDirective

class UtilityGenerator(
    private val directive: CustomizeDirective<HaskellContext, HaskellSettings>,
) : Runnable {
    companion object {
        private val UTILITY_FILE =
            this.javaClass.getResourceAsStream("/utility.hs")
    }

    override fun run() {
        directive
            .context()
            .writerDelegator()
            .useSymbolWriter(directive.context().utilitySymbol) { writer ->
                val contents =
                    UTILITY_FILE?.bufferedReader()?.readText()
                        ?: throw IllegalStateException("Utility file not found.")

                writer.addExport("SerDe (..)")
                writer.addExport("setMethod")
                writer.addExport("setPath")
                writer.addExport("serHeader")
                writer.addExport("serHeaderMap")
                writer.addExport("serQuery")
                writer.addExport("serQueryMap")
                writer.addExport("SerializeBody (..)")
                writer.addExport("serField")
                writer.addExport("FromResponseParser (..)")
                writer.addExport("deSerHeader")
                writer.addExport("deSerHeaderMap")
                writer.addExport("deSerField")
                writer.addExport("DeSerializeBody (..)")
                writer.addExport("IntoRequestBuilder (..)")
                writer.addExport("OperationError (..)")
                writer.addExport("RawRequest (..)")
                writer.addExport("RawResponse (..)")
                writer.addExport("HttpMetadata (..)")
                writer.addExport("DynAuth (..)")
                writer.addExport("BasicAuth (..)")
                writer.addExport("BearerAuth (..)")
                writer.addExport("runOperation")

                writer.addDependency(HaskellSymbol.Map)
                writer.addDependency(Http.HTTPDate)
                writer.addDependency(Http.UTCTime)
                writer.addDependency(Http.POSIXTime)
                writer.addDependency(HaskellSymbol.ByteString)
                writer.addDependency(HaskellDependencies.CaseInsensitive)
                writer.addDependency(HaskellDependencies.MTL)
                writer.writeWithNoFormatting(contents)
                writer.trimBlankLines()
            }
    }
}
