@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.HaskellShapeDirective
import `in`.juspay.smithy.haskell.client.codegen.language.ClientRecord
import software.amazon.smithy.codegen.core.CodegenException
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.traits.HttpBearerAuthTrait
import java.util.function.Consumer

class ServiceGenerator<T : HaskellShapeDirective<ServiceShape>> : Consumer<T> {
    override fun accept(directive: T) {
        val context = directive.context()
        val service = directive.service()
        val symbol = directive.symbol()
        val unsupportedAuthTrait = service.allTraits.toList().find {
            it.first.name.contains("http", ignoreCase = true) &&
                it.first.name.contains("auth", ignoreCase = true) &&
                it.first != HttpBearerAuthTrait.ID
        }

        unsupportedAuthTrait?.let {
            throw CodegenException("Unsupported HTTP Auth: ${it.first}")
        }

        context.writerDelegator().useShapeWriter(service) { writer ->
            val record = ClientRecord(
                service,
                directive.symbolProvider()
            ).toRecord()
            writer.writeRecord(record)
            writer.addExport(symbol.name)
            writer.exposeModule()
            record.fields.forEach { writer.addExport(it.name) }
            BuilderGenerator(record, symbol, writer).run()
        }
    }
}
