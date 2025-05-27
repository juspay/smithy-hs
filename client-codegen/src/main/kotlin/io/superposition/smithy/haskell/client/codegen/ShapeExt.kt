package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.CodegenException
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.model.traits.*

fun Shape.isHttpHeader(): Boolean = this.hasTrait(HttpHeaderTrait.ID)
fun Shape.isHttpQueryMap(): Boolean = this.hasTrait(HttpQueryParamsTrait.ID)
fun Shape.isHttpQuery(): Boolean = this.hasTrait(HttpQueryTrait.ID)
fun Shape.isHttpLabel(): Boolean = this.hasTrait(HttpLabelTrait.ID)
fun Shape.isHttpPayload(): Boolean = this.hasTrait(HttpPayloadTrait.ID)
fun Shape.isHttpResponseCode(): Boolean = this.hasTrait(HttpResponseCodeTrait.ID)
fun Shape.isHttpPrefixHeaders(): Boolean = this.hasTrait(HttpPrefixHeadersTrait.ID)

fun Shape.isInputShape(): Boolean = this.hasTrait(InputTrait.ID)

fun OperationShape.inputShape(model: Model): StructureShape {
    return model.expectShape(this.input.get(), StructureShape::class.java)
}

fun StructureShape.expectMember(member: String): MemberShape {
    return this.getMember(member).orElseThrow { CodegenException("$member did not exist on $this") }
}

fun MemberShape.jsonName(): String {
    return this.getTrait(JsonNameTrait::class.java).map { it.value }.orElse(this.memberName)
}

fun MemberShape.enumValue(): String {
    return this.getTrait(EnumValueTrait::class.java).map { it.expectStringValue() }.orElse(this.memberName)
}
