package io.superposition.smithy.haskell.client.codegen.language

import io.superposition.smithy.haskell.client.codegen.HaskellSymbol
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.traits.HttpBearerAuthTrait

data class Record(
    val name: String,
    val fields: List<Field>,
) {
    val defaultDerives = listOf(HaskellSymbol.Generic)
    data class Field(val name: String, val symbol: Symbol)
}

class ClientRecord(service: ServiceShape, symbolProvider: SymbolProvider) {
    val name = symbolProvider.toSymbol(service).name
    val uri = Record.Field("endpointUri", HaskellSymbol.Http.Uri)
    val httpManager = Record.Field("httpManager", HaskellSymbol.Http.Manager)
    val token = if (service.hasTrait(HttpBearerAuthTrait.ID)) {
        Record.Field("token", HaskellSymbol.Text)
    } else {
        null
    }

    fun toRecord(): Record {
        val fields = mutableListOf(uri, httpManager)
        token?.let { fields.add(it) }
        return Record(name, fields)
    }
}
