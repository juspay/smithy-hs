package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.*

// TODO
// Handle nullability.

class HaskellSymbolProvider(
    val model: Model,
    val serviceShape: ServiceShape,
    val pkgName: String
) : SymbolProvider {
    override fun toSymbol(shape: Shape): Symbol {
        val builder = Symbol.Builder()
            .name(toHaskellTypeName(shape))
            .namespace(getNamespace(shape), ".")
            .definitionFile(getDefinitionFile(shape))

        // Add references to other shapes if needed
        addReferences(builder, shape)

        return builder.build()
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
            "$pkgName.${namespace.replace('.', '.')}"
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
