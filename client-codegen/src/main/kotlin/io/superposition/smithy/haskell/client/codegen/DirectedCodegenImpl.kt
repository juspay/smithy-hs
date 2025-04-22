package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.WriterDelegator
import software.amazon.smithy.codegen.core.directed.CreateContextDirective
import software.amazon.smithy.codegen.core.directed.CreateSymbolProviderDirective
import software.amazon.smithy.codegen.core.directed.DirectedCodegen
import software.amazon.smithy.codegen.core.directed.GenerateEnumDirective
import software.amazon.smithy.codegen.core.directed.GenerateErrorDirective
import software.amazon.smithy.codegen.core.directed.GenerateIntEnumDirective
import software.amazon.smithy.codegen.core.directed.GenerateServiceDirective
import software.amazon.smithy.codegen.core.directed.GenerateStructureDirective
import software.amazon.smithy.codegen.core.directed.GenerateUnionDirective

public class DirectedCodegenImpl :
    DirectedCodegen<HaskellContext, HaskellSettings, HaskellIntegration> {

    override fun createSymbolProvider(
        directive: CreateSymbolProviderDirective<HaskellSettings>
    ): SymbolProvider {
        return HaskellSymbolProvider(
            directive.model(),
            directive.service(),
            directive.settings().packageName
        )
    }

    override fun createContext(
        directive: CreateContextDirective<HaskellSettings, HaskellIntegration>
    ): HaskellContext {
        val sp = directive.symbolProvider()
        val writerDelegator =
            WriterDelegator(
                directive.fileManifest(),
                sp,
                HaskellWriter.Factory(directive.settings())
            )

        return HaskellContext(
            model = directive.model(),
            settings = directive.settings(),
            symbolProvider = sp,
            fileManifest = directive.fileManifest(),
            writerDeligator = writerDelegator,
            integrations = directive.integrations()
        )
    }

    override fun generateService(
        directive: GenerateServiceDirective<HaskellContext, HaskellSettings>
    ) {
        val context = directive.context()
        val service = directive.service()
        val settings = directive.settings()

        // Generate service client code
        val symbol = context.symbolProvider().toSymbol(service)
        context.writerDelegator().useShapeWriter(service) { writer ->
            // Write service client implementation
            writer.write("-- Service client for ${service.id.name}")
            writer.write("module ${settings.packageName}.${service.id.name} where")
            writer.write("")
            writer.write("-- TODO: Implement service client")
        }
    }

    override fun generateStructure(
        directive: GenerateStructureDirective<HaskellContext, HaskellSettings>
    ) {
        val context = directive.context()
        val structure = directive.shape()

        // Generate structure code
        context.writerDelegator().useShapeWriter(structure) { writer ->
            // Write structure implementation
            writer.write("data ${structure.id.name} = ${structure.id.name}")
            writer.write("  { ")

            // Add structure members
            structure.members().forEach { member ->
                val s = context.symbolProvider().toSymbol(member)
                writer.write("  ${member.memberName} :: ${s.name}")
            }

            writer.write("  } deriving (Show, Eq)")
        }
    }

    override fun generateError(directive: GenerateErrorDirective<HaskellContext, HaskellSettings>) {
        val context = directive.context()
        val error = directive.shape()

        context.writerDelegator().useShapeWriter(error) { writer ->
            // Write error implementation
            writer.write("-- Error for ${error.id.name}")
            writer.write("data ${error.id.name} = ${error.id.name}")
            writer.write("  { ")

            // Add error members
            error.members().forEach { member ->
                val memberSymbol = context.symbolProvider().toSymbol(member)
                writer.write("  ${member.memberName} :: ${memberSymbol.name}")
            }

            writer.write("  } deriving (Show, Eq)")
            writer.write("")
            writer.write("instance Exception ${error.id.name}")
        }
    }

    override fun generateUnion(directive: GenerateUnionDirective<HaskellContext, HaskellSettings>) {
        val context = directive.context()
        val union = directive.shape()

        context.writerDelegator().useShapeWriter(union) { writer ->
            // Write union implementation
            writer.write("-- Union for ${union.id.name}")
            writer.write("data ${union.id.name}")
            writer.write(" = ")

            // Add union members
            union.members().forEachIndexed { index, member ->
                val s = context.symbolProvider().toSymbol(member)
                val prefix = if (index == 0) "" else "  | "
                writer.write("${prefix}${member.memberName} ${s.name}")
            }

            writer.write("  deriving (Show, Eq)")
        }
    }

    override fun generateEnumShape(
        directive: GenerateEnumDirective<HaskellContext, HaskellSettings>
    ) {
        val context = directive.context()
        val enum = directive.shape()
        context.writerDelegator().useShapeWriter(enum) { writer ->
            // Write enum implementation
            writer.write("-- Enum for ${enum.id.name}")
            writer.write("data ${enum.id.name}")
            writer.write(" = ")

            // Add enum members
            enum.members().forEachIndexed { index, member ->
                val prefix = if (index == 0) "" else "  | "
                val enumName = member.memberName.replaceFirstChar { it.uppercase() }
                writer.write("${prefix}${enumName}")
            }

            writer.write("  deriving (Show, Eq, Enum)")
        }
    }

    override fun generateIntEnumShape(
        directive: GenerateIntEnumDirective<HaskellContext, HaskellSettings>
    ) {
        val context = directive.context()
        // TODO Handle error if not present.
        val intEnum = directive.shape().asIntEnumShape().get()

        context.writerDelegator().useShapeWriter(intEnum) { writer ->
            // Write int enum implementation
            writer.write("-- IntEnum for ${intEnum.id.name}")
            writer.write("data ${intEnum.id.name}")
            writer.write(" = ")

            // Add int enum members
            intEnum.enumValues.entries.toList().forEachIndexed() { i, (name, value) ->
                val prefix = if (i == 0) "" else "  | "
                val n = name.replaceFirstChar { it.uppercase() }
                writer.write("$prefix$n -- $value")
            }

            writer.write("  deriving (Show, Eq)")

            // Add conversion functions
            writer.write("")
            writer.write("toInt :: ${intEnum.id.name} -> Int")
            intEnum.enumValues.forEach { (name, value) ->
                val n = name.replaceFirstChar { it.uppercase() }
                writer.write("toInt $n = $value")
            }

            writer.write("")
            writer.write("fromInt :: Int -> Maybe ${intEnum.id.name}")
            intEnum.enumValues.forEach { (name, value) ->
                val n = name.replaceFirstChar { it.uppercase() }
                writer.write("fromInt $value = Just $n")
            }
            writer.write("fromInt _ = Nothing")
        }
    }
}
