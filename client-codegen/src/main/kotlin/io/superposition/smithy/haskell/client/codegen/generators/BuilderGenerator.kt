package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.isMaybe
import io.superposition.smithy.haskell.client.codegen.toMaybe
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.utils.CaseUtils

class BuilderGenerator(
    val shape: StructureShape,
    val symbol: Symbol,
    val symbolProvider: SymbolProvider,
    val writer: HaskellWriter
) : Runnable {
    private data class BuilderStateMember(
        val name: String,
        val symbol: Symbol,
        val inputShape: MemberShape,
        val inputSymbol: Symbol
    )

    private val builderName = "${shape.id.name}Builder"
    private val stateName = "${builderName}State"
    private val builderStateMembers = shape.members().map {
        val name = "${symbolProvider.toMemberName(it)}BuilderState"
        val symbol = symbolProvider.toSymbol(it).toMaybe()
        return@map BuilderStateMember(name, symbol, it, symbolProvider.toSymbol(it))
    }

    override fun run() {
        writer.pushState()
        writer.putContext("builderState", Runnable(::builderStateSection))
        writer.putContext("defaultBuilderState", Runnable(::defaultBuilderState))
        writer.putContext("builderSetters", Runnable(::builderSetters))
        writer.putContext("builderFunction", Runnable(::builderFunction))
        writer.write(
            """

           #{builderState:C}

           #{defaultBuilderState:C}

           newtype $builderName a = $builderName {
               run$builderName :: $stateName -> ($stateName, a)
           }

           instance #{functor:T} $builderName where
               fmap f ($builderName g) =
                   $builderName (\s -> let (s', a) = g s in (s', f a))

           instance #{applicative:T} $builderName where
               pure a = $builderName (\s -> (s, a))
               ($builderName f) <*> ($builderName g) = $builderName (\s ->
                   let (s', h) = f s
                       (s'', a) = g s'
                   in (s'', h a))

           instance #{monad:T} $builderName where
               ($builderName f) >>= g = $builderName (\s ->
                   let (s', a) = f s
                       ($builderName h) = g a
                   in h s')
           #{builderSetters:C}

           #{builderFunction:C}
            """.trimIndent()
        )
        writer.addExport(builderName)
        writer.popState()
    }

    private fun builderStateSection() {
        writer.openBlock("data $stateName = $stateName {", "}") {
            builderStateMembers.forEach {
                writer.write("${it.name} :: #T,", it.symbol)
            }
        }
    }

    private fun defaultBuilderState() {
        val fn = "defaultBuilderState"
        writer.write("$fn :: $stateName")
        writer.openBlock("$fn = $stateName {", "}") {
            builderStateMembers.forEach {
                writer.write("${it.name} = #{nothing:T},")
            }
        }
    }

    @Suppress("MaxLineLength")
    private fun builderSetters() {
        builderStateMembers.forEach {
            val im = it.inputShape
            val fn = CaseUtils.toCamelCase("set ${im.memberName}")
            writer.addExport(fn)
            writer.putContext("isMaybe", it.inputSymbol.isMaybe())
            writer.write(
                """

                $fn :: #T -> $builderName ()
                $fn value =
                   $builderName (\s -> (s { ${it.name} = #{^isMaybe}#{just:T} #{/isMaybe}value }, ()))
                """.trimIndent(),
                symbolProvider.toSymbol(it.inputShape)
            )
        }
    }

    @Suppress("MaxLineLength")
    private fun builderFunction() {
        val fn = "build"
        writer.addExport(fn)
        writer.write("$fn :: $builderName () -> #{either:T} #{text:T} ${shape.id.name}")
        writer.openBlock("$fn builder = do", "") {
            builderStateMembers.forEach {
                val mn = it.inputShape.memberName
                val e = "\"$symbol.$mn is a required property.\""
                if (it.inputSymbol.isMaybe()) {
                    writer.write("$mn' <- #{right:T} (${it.name} builder)")
                } else {
                    writer.write("$mn' <- Data.Maybe.maybe (#{left:T} $e) #{right:T} (${it.name} builder)")
                }
            }
            writer.openBlock("#{right:T} (#T { ", "})", symbol) {
                builderStateMembers.forEach {
                    writer.write("${it.inputShape.memberName} = ${it.inputShape.memberName}',")
                }
            }
        }
    }
}
