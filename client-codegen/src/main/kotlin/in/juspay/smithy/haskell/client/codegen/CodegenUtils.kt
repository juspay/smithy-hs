package `in`.juspay.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.ReservedWords
import software.amazon.smithy.codegen.core.ReservedWordsBuilder
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.utils.CaseUtils
import java.net.URL

object CodegenUtils {
    // TODO Refer smithy-java for resource loading
    private final val RESERVED_WORDS_FILE: URL =
        this.javaClass.getResource("/reserved-words.txt")!!
    final val SHAPE_ESCAPER: ReservedWords = ReservedWordsBuilder()
        .loadCaseInsensitiveWords(RESERVED_WORDS_FILE) { word -> "$word'" }
        .build()

    fun getValidName(shape: Shape): String {
        val baseName: String = shape.id.name
        return SHAPE_ESCAPER.escape(baseName)
    }

    /**
     * Checks if the provided shape has the input trait.
     *
     * @param shape The shape to check
     * @return true if the shape has the input trait, false otherwise
     */
    fun isInputShape(shape: Shape): Boolean {
        return shape.hasTrait("smithy.api#input")
    }

    fun toModName(s: String): String {
        // split on . and then convert to pascal case and then join with .
        return s.split('.').joinToString(".") { part ->
            CaseUtils.toPascalCase(part)
        }
    }

    /** @param begin - Earliest usable version(>=)
     * @param end - Lowest version which can't be used.(<)
     * **/
    fun depRange(begin: String, end: String) = ">= $begin && < $end"

    fun toHaskellHttpMethod(method: String): Symbol {
        return when (method) {
            "GET" -> Http.Get
            "POST" -> Http.Post
            "PUT" -> Http.Put
            "DELETE" -> Http.Delete
            "PATCH" -> Http.Patch
            "HEAD" -> Http.Head
            "OPTIONS" -> Http.Options
            "CONNECT" -> Http.Connect
            "TRACE" -> Http.Trace
            else -> Http.Custom
        }
    }

    fun getSetterName(fieldName: String): String {
        return CaseUtils.toCamelCase("set $fieldName")
    }

    val String.dq: String
        get() = "\"$this\""

    val comma: String
        get() = ",".dq
}
