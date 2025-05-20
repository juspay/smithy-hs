package io.superposition.smithy.haskell.client.codegen.language

import software.amazon.smithy.codegen.core.Symbol

data class Record(val name: String, val fields: List<Field>) {
    data class Field(val name: String, val symbol: Symbol)
}
