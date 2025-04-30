package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolDependency
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.*
import java.util.logging.Logger

// TODO
// Handle nullability.
// Handle sparse lists.
// Handle trait based validation on primitives.

class HaskellSymbolProvider(
    val model: Model,
    val serviceShape: ServiceShape,
    val pkgName: String
) : SymbolProvider {
    val logger = Logger.getLogger(this.javaClass.name)

    override fun toSymbol(shape: Shape): Symbol {
        val builder = Symbol.Builder()
            .name(toHaskellTypeName(shape))
            .namespace(getNamespace(shape), ".")
        // Add dependencies for primitives.
        addDependencies(builder, shape)
        // Add references to other shapes if needed.
        addReferences(builder, shape)

        when (shape) {
            is StructureShape, is UnionShape, is EnumShape -> {
                builder.definitionFile("src/Lib.hs")
            }
        }

        logger.info("Building symbol for shape: ${shape.id.name}")

        return builder.build()
    }

    private fun addDependencies(builder: Symbol.Builder, shape: Shape) {
        // REVIEW Should the Hat(^) be in the version? Can use an ADT instead.
        when (shape) {
            is StringShape -> {
                builder.addDependency(
                    SymbolDependency.builder()
                        .packageName("text")
                        .version("^1.2")
                        .build()
                )
            }

            is BlobShape -> {
                builder.addDependency(
                    SymbolDependency.builder()
                        .packageName("bytestring")
                        .version("^0.11")
                        .build()
                )
            }

            is TimestampShape -> {
                builder.addDependency(
                    SymbolDependency.builder()
                        .packageName("time")
                        .version("^1.9")
                        .build()
                )
            }

            is MapShape -> {
                builder.addDependency(
                    SymbolDependency.builder()
                        .packageName("containers")
                        .version("^0.6")
                        .build()
                )
            }

            is ListShape -> {
                builder.addDependency(
                    SymbolDependency.builder()
                        .packageName("base")
                        .version("^4.12")
                        .build()
                )
            }
        }
    }

    private fun toHaskellTypeName(shape: Shape): String {
        // Convert shape name to PascalCase for Haskell type names
        return when (shape) {
            is StructureShape, is UnionShape, is EnumShape -> shape.id.name
            is StringShape -> "Text"
            is BooleanShape -> "Bool"
            is ByteShape, is ShortShape, is IntegerShape -> "Int"
            is LongShape, is BigIntegerShape -> "Integer"
            is FloatShape, is DoubleShape, is BigDecimalShape -> "Double"
            is BlobShape -> "ByteString"
            is TimestampShape -> "UTCTime"
            is ListShape -> "List"
            is MapShape -> "Map"
            else -> error("Unknown shape type $shape encountered while creating a symbol.")
        }
    }

    private fun getNamespace(shape: Shape): String {
        // Create appropriate module namespace based on shape's namespace
        val namespace = shape.id.namespace

        // Convert namespace to Haskell module path
        return if (namespace.isEmpty()) {
            pkgName
        } else {
            "$pkgName.$namespace"
        }
    }

    private fun getDefinitionFile(shape: Shape): String {
        // Convert shape name to file path
        val typeName = toHaskellTypeName(shape)
        return "$typeName.hs"
    }

    private fun addReferences(builder: Symbol.Builder, shape: Shape) {
        when (shape) {
            is ListShape -> {
                // Add reference to member type
                val memberTarget = model.expectShape(shape.member.target)
                builder.addReference(toSymbol(memberTarget))
            }

            is MapShape -> {
                // Add references to key and value types
                val keyTarget = model.expectShape(shape.key.target)
                val valueTarget = model.expectShape(shape.value.target)
                builder.addReference(toSymbol(keyTarget))
                builder.addReference(toSymbol(valueTarget))
            }

            is StructureShape -> {
                // Add references to all member types
                shape.members().forEach { member ->
                    val memberTarget = model.expectShape(member.target)
                    builder.addReference(toSymbol(memberTarget))
                }
            }

            is UnionShape -> {
                // Add references to all member types
                shape.members().forEach { member ->
                    val memberTarget = model.expectShape(member.target)
                    builder.addReference(toSymbol(memberTarget))
                }
            }
        }
    }
}
