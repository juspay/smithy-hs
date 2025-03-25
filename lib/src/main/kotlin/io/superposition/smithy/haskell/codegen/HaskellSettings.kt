import software.amazon.smithy.model.shapes.ShapeId

public data class HaskellSettings(
        val shapeId: ShapeId,
        val packageName: String,
        val edition: String
)
