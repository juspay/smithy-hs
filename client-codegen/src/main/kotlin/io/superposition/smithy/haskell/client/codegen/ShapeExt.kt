package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.traits.*

val MemberShape.jsonName: String
    get() = this.getTrait(JsonNameTrait::class.java).map { it.value }
        .orElse(this.memberName)

val MemberShape.enumValue: String
    get() = this.getTrait(EnumValueTrait::class.java).map { it.expectStringValue() }
        .orElse(this.memberName)

fun MemberShape.isMemberListShape(model: Model): Boolean {
    return model.expectShape(this.target).isListShape
}
