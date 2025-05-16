package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.model.node.ObjectNode
import software.amazon.smithy.model.shapes.ShapeId

data class HaskellSettings(
    val service: ShapeId,
    val packageName: String,
    val edition: String,
    val version: String
) {
    companion object {
        fun fromNode(settings: ObjectNode): HaskellSettings {
            val builder = HaskellSettingsBuilder()
            settings.expectStringMember("service", {
                    id ->
                builder.serviceShapeId = ShapeId.from(id)
            })
                .expectStringMember("packageName") { pname -> builder.packageName = pname }
                .expectStringMember("edition") { e -> builder.edition = e }
                .expectStringMember("version") { e -> builder.version = e }

            return HaskellSettings(
                builder.serviceShapeId!!,
                builder.packageName!!,
                builder.edition!!,
                builder.version!!
            )
        }
    }
}

private data class HaskellSettingsBuilder(
    var serviceShapeId: ShapeId? = null,
    var packageName: String? = null,
    var edition: String? = null,
    var version: String? = null
)
