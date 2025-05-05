package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.ReservedWords
import software.amazon.smithy.codegen.core.ReservedWordsBuilder
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.utils.CaseUtils
import software.amazon.smithy.utils.StringUtils
import java.net.URL

class CodegenUtils {
    companion object {
        // TODO Refer smithy-java for resource loading
        private final val RESERVED_WORDS_FILE: URL = this.javaClass.getResource("/reserved-words.txt")!!
        private final val SHAPE_ESCAPER: ReservedWords = ReservedWordsBuilder()
            .loadCaseInsensitiveWords(RESERVED_WORDS_FILE) { word -> word + "Shape" }
            .build()

        fun getDefaultName(shape: Shape, service: ServiceShape): String {
            val baseName: String = shape.id.getName(service)

            val unescaped: String = if (baseName.contains("_")) {
                CaseUtils.toPascalCase(shape.id.name)
            } else {
                StringUtils.capitalize(baseName)
            }

            return SHAPE_ESCAPER.escape(unescaped)
        }

        fun toModName(s: String): String {
            // split on . and then convert to pascal case and then join with .
            return s.split('.').joinToString(".") { part ->
                CaseUtils.toPascalCase(part)
            }
        }
    }
}
