@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.toMaybe
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.model.traits.RequiredTrait

@Suppress("MaxLineLength")
class StructureGenerator<T : ShapeDirective<StructureShape, HaskellContext, HaskellSettings>>(
    private val directive: T
) : Runnable {

    override fun run() {
        val symbolProvider = directive.symbolProvider()
        val shape = directive.shape()
        val symbol = directive.symbol()
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            writer.openBlock("data #T = #T {", "}", symbol, symbol) {
                shape.members().map {
                    val mName = symbolProvider.toMemberName(it)
                    var mSymbol = symbolProvider.toSymbol(it)
                    if (!it.hasTrait(RequiredTrait.ID)) {
                        mSymbol = mSymbol.toMaybe()
                    }
                    writer.write("$mName :: #T,", mSymbol)
                }
            }
            writer.addExport(symbol.name)
            shape.members().forEach {
                writer.addExport(it.memberName)
            }
            writer.exposeModule()
            writer.write("#C", BuilderGenerator(shape, symbol, symbolProvider, writer))
        }
    }
}
