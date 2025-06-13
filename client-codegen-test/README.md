# Client Codegen Test

This module is primarily used during development to test changes to the code generator.
When you make changes to the Haskell codegen implementation, you can run the build in this module to see how those changes affect the generated Haskell code.

We are using the smithy development plugin which automatically runs as part of the `build` task. You can see the generated code in
`output/haskell-client-codegen`.

Apart from that, we have a Haskell module which imports this generated client & runs tests on it in `hs-it`.
