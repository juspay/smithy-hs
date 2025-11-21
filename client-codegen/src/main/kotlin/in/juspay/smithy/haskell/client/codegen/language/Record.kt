package `in`.juspay.smithy.haskell.client.codegen.language

import `in`.juspay.smithy.haskell.client.codegen.HaskellContext
import `in`.juspay.smithy.haskell.client.codegen.HaskellSymbol
import `in`.juspay.smithy.haskell.client.codegen.toMaybe
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.traits.HttpBasicAuthTrait
import software.amazon.smithy.model.traits.HttpBearerAuthTrait

data class Record(
    val name: String,
    val fields: List<Field>,
    val derives: List<Symbol> = listOf(),
) {
    val defaultDerives = derives + listOf(HaskellSymbol.Generic)

    data class Field(val name: String, val symbol: Symbol)
}

class ClientRecord(
    val service: ServiceShape,
    symbolProvider: SymbolProvider,
    context: HaskellContext,
) {
    val utils = context.utilitySymbol
    val name = symbolProvider.toSymbol(service).name
    val uri = Record.Field("endpointUri", HaskellSymbol.Http.Uri)
    val httpManager =
        Record.Field("httpManager", HaskellSymbol.Http.Manager)

    fun toRecord(): Record {
        val fields = mutableListOf(uri, httpManager)
        if (service.hasTrait(HttpBearerAuthTrait.ID)) {
            fields.add(
                Record.Field(
                    "bearerAuth",
                    Symbol.builder()
                        .name("BearerAuth")
                        .namespace(utils.namespace, ".")
                        .build().toMaybe(),
                ),
            )
        }
        if (service.hasTrait(HttpBasicAuthTrait.ID)) {
            fields.add(
                Record.Field(
                    "basicAuth",
                    Symbol.builder()
                        .name("BasicAuth")
                        .namespace(utils.namespace, ".")
                        .build().toMaybe(),
                ),
            )
        }
        return Record(name, fields)
    }

    companion object {
        val AUTHORIZATION_FIELD = "authorization"
    }
}
