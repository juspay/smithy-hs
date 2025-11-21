@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.HaskellShapeDirective
import `in`.juspay.smithy.haskell.client.codegen.language.ClientRecord
import software.amazon.smithy.model.knowledge.ServiceIndex
import software.amazon.smithy.model.shapes.ServiceShape
import java.util.function.Consumer

class ServiceGenerator<T : HaskellShapeDirective<ServiceShape>> : Consumer<T> {
    override fun accept(directive: T) {
        val context = directive.context()
        val service = directive.service()
        val symbol = directive.symbol()

        context.writerDelegator().useShapeWriter(service) { writer ->
            val record =
                ClientRecord(
                    service,
                    directive.symbolProvider(),
                    context,
                ).toRecord()
            writer.writeRecord(record)
            writer.addExport(symbol.name)
            writer.putContext("utils", context.utilitySymbol)
            // re-exports for http types :( FIXME!
            writer.addExport(writer.format("#{utils:N}.RawRequest"))
            writer.addExport(writer.format("#{utils:N}.requestMethod"))
            writer.addExport(writer.format("#{utils:N}.requestPath"))
            writer.addExport(writer.format("#{utils:N}.requestQuery"))
            writer.addExport(writer.format("#{utils:N}.requestHeaders"))
            writer.addExport(writer.format("#{utils:N}.requestBody"))
            writer.addExport(writer.format("#{utils:N}.RawResponse"))
            writer.addExport(writer.format("#{utils:N}.responseStatus"))
            writer.addExport(writer.format("#{utils:N}.responseHeaders"))
            writer.addExport(writer.format("#{utils:N}.responseBody"))
            writer.addExport(writer.format("#{utils:N}.HttpMetadata"))
            writer.addExport(writer.format("#{utils:N}.rawRequest"))
            writer.addExport(writer.format("#{utils:N}.rawResponse"))
            writer.addExport(writer.format("#{utils:N}.BearerAuth(..)"))
            writer.addExport(writer.format("#{utils:N}.BasicAuth(..)"))
            writer.exposeModule()
            record.fields.forEach { writer.addExport(it.name) }
            BuilderGenerator(
                record,
                symbol,
                writer,
            ).run()

            writer.write("getAuth :: ${record.name} -> Maybe #{utils:N}.DynAuth")
            writer.openBlock("getAuth client = (Nothing", "") {
                val index = ServiceIndex.of(directive.model())
                index.getEffectiveAuthSchemes(service).entries.forEach {
                    val field =
                        it.key.name
                            .removePrefix("http")
                            .replaceFirstChar { it.lowercase() }
                    writer.write(
                        "#{applicative:N}.<|> (#{utils:N}.DynAuth <$> ($field client))",
                    )
                }
                writer.write(")")
            }
            writer.addExport("getAuth")
        }
    }
}
