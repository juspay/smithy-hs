@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package `in`.juspay.smithy.haskell.client.codegen.generators

import `in`.juspay.smithy.haskell.client.codegen.HaskellShapeDirective
import `in`.juspay.smithy.haskell.client.codegen.HaskellSymbol
import `in`.juspay.smithy.haskell.client.codegen.fieldName
import `in`.juspay.smithy.haskell.client.codegen.language.Record
import software.amazon.smithy.model.shapes.StructureShape

class StructureGenerator<T : HaskellShapeDirective<StructureShape>>(
    private val directive: T
) : Runnable {
    private val shape = directive.shape()
    private val symbol = directive.symbol()
    private val symbolProvider = directive.symbolProvider()

    override fun run() {
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            val template = """
            #{record:C|}

            #{serializer:C|}
            instance #{utility:N}.SerializeBody ${symbol.name}

            #{deserializer:C|}

            #{builder:C|}
            """.trimIndent()

            val record = Record(
                symbol.name,
                shape.members()
                    .map {
                        Record.Field(
                            it.fieldName,
                            symbolProvider.toSymbol(it)
                        )
                    },
                listOf(HaskellSymbol.Show, HaskellSymbol.Eq),
            )

            writer.pushState()
            writer.putContext("record", Runnable { writer.writeRecord(record) })
            writer.putContext(
                "serializer",
                StructureSerializerGenerator(directive, writer)
            )
            writer.putContext(
                "deserializer",
                StructureDeserializerGenerator(directive, writer)
            )
            writer.putContext("utility", directive.context().utilitySymbol)
            writer.putContext("builder", BuilderGenerator(record, symbol, writer))
            writer.write(template)
            writer.addExport(symbol.name)
            shape.members().forEach {
                writer.addExport(it.fieldName)
            }
            writer.exposeModule()
            writer.popState()
        }
    }
}
