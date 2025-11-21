package `in`.juspay.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.CodegenException
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.*
import software.amazon.smithy.model.traits.RequiredTrait
import software.amazon.smithy.model.traits.TimestampFormatTrait
import software.amazon.smithy.model.traits.Trait
import java.util.logging.Logger
import kotlin.jvm.optionals.getOrNull

// TODO
// Handle sparse lists.

fun <T : Shape> T.toSymbolBuilder(): Symbol.Builder =
    Symbol.builder().name(this.toShapeId().name)

private fun Symbol.Builder.projectNamespace(namespace: String): Symbol.Builder {
    val path = namespace.replace(".", "/") + ".hs"
    return this.namespace(namespace, ".").definitionFile(path)
}

@Suppress("UNCHECKED_CAST")
private fun <T : Trait> Shape.findTraitOrNull(id: ShapeId): T? =
    (this.findTrait(id).getOrNull() as T?)

@Suppress("TooManyFunctions")
class HaskellSymbolProvider(
    private val model: Model,
    private val service: ServiceShape,
    namespace: String,
) : SymbolProvider,
    ShapeVisitor<Symbol> {
    private val logger: Logger = Logger.getLogger(this.javaClass.name)
    private val namespace: String = CodegenUtils.toModName(namespace)

    override fun toSymbol(shape: Shape): Symbol {
        val symbol: Symbol = shape.accept(this)
        logger.info("Creating symbol from $shape:$symbol")
        return symbol
    }

    override fun structureShape(shape: StructureShape): Symbol {
        val name = CodegenUtils.getValidName(shape)
        val symbol =
            Symbol.builder().name(name).putProperty(SymbolProperties.IS_PRIMITIVE, false)
                .projectNamespace("$namespace.Model.$name")

        return symbol.build()
    }

    override fun memberShape(shape: MemberShape): Symbol {
        val target = model.getShape(shape.target).orElseThrow<CodegenException> {
            CodegenException("Could not find shape ${shape.target} targeted by $shape")
        }

        val parent = model.getShape(shape.container).orElseThrow<CodegenException> {
            CodegenException("Could not find shape ${shape.container} parent of $shape")
        }

        val symbol = when (target) {
            is TimestampShape -> {
                // Member-shapes are allowed to override the ts-format.
                val trait =
                    shape.findTraitOrNull<TimestampFormatTrait>(TimestampFormatTrait.ID)
                timestampShape(target, trait)
            }

            else -> toSymbol(target)
        }

        return symbol.let {
            if ((parent is StructureShape) && !shape.hasTrait(RequiredTrait.ID)) {
                it.toMaybe()
            } else {
                it
            }
        }
    }

    override fun booleanShape(shape: BooleanShape): Symbol = Symbol.builder().name("Bool")
        .putProperty(SymbolProperties.IS_PRIMITIVE, true).build()

    override fun integerShape(shape: IntegerShape): Symbol =
        Symbol.builder().name("Int32").namespace("Data.Int", ".").build()

    override fun stringShape(shape: StringShape): Symbol = HaskellSymbol.Text

    override fun doubleShape(shape: DoubleShape): Symbol = Symbol.builder().name("Double")
        .putProperty(SymbolProperties.IS_PRIMITIVE, true).build()

    override fun floatShape(shape: FloatShape?): Symbol = Symbol.builder().name("Float")
        .putProperty(SymbolProperties.IS_PRIMITIVE, true).build()

    override fun longShape(shape: LongShape): Symbol =
        Symbol.builder().name("Int64").namespace("Data.Int", ".")
            .putProperty(SymbolProperties.IS_PRIMITIVE, false).build()

    override fun shortShape(shape: ShortShape?): Symbol =
        Symbol.builder().name("Int16").namespace("Data.Int", ".")
            .putProperty(SymbolProperties.IS_PRIMITIVE, false).build()

    override fun byteShape(shape: ByteShape?): Symbol =
        Symbol.builder().name("Int8").namespace("Data.Int", ".")
            .putProperty(SymbolProperties.IS_PRIMITIVE, false).build()

    override fun bigDecimalShape(shape: BigDecimalShape?): Symbol {
        // TODO Add via `decimal` package.
        error("BigDecimalShape is not supported")
    }

    override fun bigIntegerShape(shape: BigIntegerShape?): Symbol =
        Symbol.builder().name("Integer").namespace("Prelude", ".")
            .putProperty(SymbolProperties.IS_PRIMITIVE, false).build()

    override fun timestampShape(shape: TimestampShape): Symbol =
        timestampShape(shape, null)

    private fun timestampShape(
        shape: TimestampShape,
        memberTrait: TimestampFormatTrait?,
    ): Symbol {
        val defaultTrait = TimestampFormatTrait(TimestampFormatTrait.EPOCH_SECONDS)
        val trait = memberTrait ?: (
            shape.findTraitOrNull<TimestampFormatTrait>(
                TimestampFormatTrait.ID,
            ) ?: defaultTrait
            )

        return when (trait.format) {
            TimestampFormatTrait.Format.DATE_TIME -> Http.UTCTime
            TimestampFormatTrait.Format.HTTP_DATE -> Http.HTTPDate
            else -> Http.POSIXTime
        }
    }

    override fun documentShape(shape: DocumentShape): Symbol = HaskellSymbol.Value

    override fun mapShape(shape: MapShape): Symbol =
        Symbol.builder().name("Map").namespace("Data.Map", ".")
            .addReference(shape.key.accept(this)).addReference(shape.value.accept(this))
            .build()

    override fun blobShape(shape: BlobShape): Symbol = HaskellSymbol.ByteString

    override fun serviceShape(shape: ServiceShape): Symbol {
        val name = "${shape.id.name}Client"
        return Symbol.builder().name(name).projectNamespace("$namespace.$name")
            .putProperty(SymbolProperties.IS_PRIMITIVE, false).build()
    }

    override fun resourceShape(shape: ResourceShape): Symbol {
        val name = CodegenUtils.getValidName(shape)
        return Symbol.builder().name(name)
            .putProperty(SymbolProperties.IS_PRIMITIVE, false)
            .projectNamespace("$namespace.Model.$name").build()
    }

    override fun operationShape(shape: OperationShape): Symbol {
        val name = CodegenUtils.getValidName(shape)
        return Symbol.builder().name(name)
            .putProperty(SymbolProperties.IS_PRIMITIVE, false)
            .projectNamespace("$namespace.Command.$name").build()
    }

    override fun enumShape(shape: EnumShape): Symbol {
        val name = CodegenUtils.getValidName(shape)
        return Symbol.builder().name(name)
            .putProperty(SymbolProperties.IS_PRIMITIVE, false)
            .projectNamespace("$namespace.Model.$name").build()
    }

    override fun unionShape(shape: UnionShape): Symbol {
        val name = CodegenUtils.getValidName(shape)
        return Symbol.builder().name(name)
            .putProperty(SymbolProperties.IS_PRIMITIVE, false)
            .projectNamespace("$namespace.Model.$name").build()
    }

    override fun listShape(shape: ListShape): Symbol =
        Symbol.builder().putProperty(SymbolProperties.IS_PRIMITIVE, false)
            .name("[]").addReference(shape.member.accept(this)).build()
}
