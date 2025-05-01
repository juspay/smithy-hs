package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.CodegenException
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolDependency
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.*
import java.lang.String.format
import java.util.logging.Logger

// TODO
// Handle nullability.
// Handle sparse lists.
// Handle trait based validation on primitives.

class HaskellSymbolProvider(
    private val model: Model,
    private val service: ServiceShape,
    private val namespace: String,
) : SymbolProvider, ShapeVisitor<Symbol> {
    private val logger: Logger = Logger.getLogger(this.javaClass.name)

    override fun toSymbol(shape: Shape): Symbol {
        val symbol: Symbol = shape.accept(this);
        logger.trace("Creating symbol from {}:{}", shape, symbol);
        return symbol;
    }

    override fun structureShape(shape: StructureShape): Symbol {
        // TODO What to do with the unit type trait?

        val name = CodegenUtils.getDefaultName(shape, service);
        return Symbol.builder()
            .name(name)
            .putProperty(SymbolProperties.IS_PRIMITIVE, false)
            .namespace(format("%s.model", namespace), ".")
            .definitionFile(format("./%s/model/%s.hs", namespace.replace(".", "/"), name))
            .build()
    }

    override fun memberShape(shape: MemberShape): Symbol {
        val target = model.getShape(shape.target)
            .orElseThrow<CodegenException> {
                CodegenException(
                    ("Could not find shape " + shape.target + " targeted by "
                        + shape)
                )
            };

        return toSymbol(target);
    }

    override fun booleanShape(shape: BooleanShape): Symbol {
        return Symbol.builder().name("Bool").putProperty(SymbolProperties.IS_PRIMITIVE, true).build()
    }

    override fun integerShape(shape: IntegerShape): Symbol {
        return Symbol.builder().name("Integer").putProperty(SymbolProperties.IS_PRIMITIVE, true).build()
    }

    override fun stringShape(shape: StringShape): Symbol {
        return Symbol.builder()
            .name("Text")
            .putProperty(SymbolProperties.IS_PRIMITIVE, false)
            .build()
    }

    override fun doubleShape(shape: DoubleShape): Symbol {
        return Symbol.builder().name("Double").putProperty(SymbolProperties.IS_PRIMITIVE, true).build()
    }

    override fun floatShape(shape: FloatShape?): Symbol {
        return Symbol.builder().name("Float").putProperty(SymbolProperties.IS_PRIMITIVE, true).build()
    }

    override fun longShape(shape: LongShape?): Symbol {
        TODO("Not yet implemented")
    }

    override fun shortShape(shape: ShortShape?): Symbol {
        TODO("Not yet implemented")
    }

    override fun byteShape(shape: ByteShape?): Symbol {
        TODO("Not yet implemented")
    }

    override fun bigDecimalShape(shape: BigDecimalShape?): Symbol {
        TODO("Not yet implemented")
    }

    override fun bigIntegerShape(shape: BigIntegerShape?): Symbol {
        TODO("Not yet implemented")
    }

    override fun timestampShape(shape: TimestampShape?): Symbol {
        TODO("Not yet implemented")
    }

    override fun documentShape(shape: DocumentShape?): Symbol {
        TODO("Not yet implemented")
    }

    override fun mapShape(shape: MapShape?): Symbol {
        TODO("Not yet implemented")
    }

    override fun blobShape(shape: BlobShape): Symbol {
        TODO("Not yet implemented")
    }

    override fun serviceShape(shape: ServiceShape): Symbol {
        TODO("Not yet implemented")
    }

    override fun resourceShape(shape: ResourceShape): Symbol {
        TODO("Not yet implemented")
    }

    override fun operationShape(shape: OperationShape): Symbol {
        TODO("Not yet implemented")
    }

    override fun enumShape(shape: EnumShape): Symbol {
        TODO("Not yet implemented")
    }

    override fun unionShape(shape: UnionShape): Symbol {
        TODO("Not yet implemented")
    }

    override fun listShape(shape: ListShape?): Symbol {
        TODO("Not yet implemented")
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
