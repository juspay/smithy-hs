package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.CodegenException
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.model.traits.HttpHeaderTrait
import software.amazon.smithy.model.traits.HttpLabelTrait
import software.amazon.smithy.model.traits.HttpPayloadTrait
import software.amazon.smithy.model.traits.HttpPrefixHeadersTrait
import software.amazon.smithy.model.traits.HttpQueryParamsTrait
import software.amazon.smithy.model.traits.HttpQueryTrait
import software.amazon.smithy.model.traits.HttpResponseCodeTrait
import software.amazon.smithy.model.traits.InputTrait

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
