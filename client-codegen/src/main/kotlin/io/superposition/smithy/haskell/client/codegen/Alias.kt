package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.directed.ShapeDirective

typealias HaskellShapeDirective<T> = ShapeDirective<T, HaskellContext, HaskellSettings>
