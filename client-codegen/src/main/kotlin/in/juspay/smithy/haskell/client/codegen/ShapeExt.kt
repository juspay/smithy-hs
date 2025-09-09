package `in`.juspay.smithy.haskell.client.codegen

import `in`.juspay.smithy.haskell.client.codegen.CodegenUtils.SHAPE_ESCAPER
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.knowledge.HttpBinding
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.traits.*

val MemberShape.jsonName: String
    get() = this.getTrait(JsonNameTrait::class.java).map { it.value }
        .orElse(this.memberName)

val MemberShape.enumValue: String
    get() = this.getTrait(EnumValueTrait::class.java).map { it.expectStringValue() }
        .orElse(this.memberName)

fun MemberShape.isMemberListShape(model: Model): Boolean =
    model.expectShape(this.target).isListShape

val MemberShape.fieldName: String
    get() = SHAPE_ESCAPER.escape(this.memberName)

val HttpBinding.fieldName: String
    get() = this.member.fieldName

val HttpBinding.jsonName: String
    get() = this.member.jsonName
