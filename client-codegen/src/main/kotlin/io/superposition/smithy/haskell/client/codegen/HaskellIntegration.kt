package io.superposition.smithy.haskell.client.codegen

import software.amazon.smithy.codegen.core.SmithyIntegration

interface HaskellIntegration : SmithyIntegration<HaskellSettings, HaskellWriter, HaskellContext>
