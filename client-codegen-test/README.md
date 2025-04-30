# Smithy Haskell Client Codegen Test

This module is primarily used during development to test changes to the code generator. When you make changes to the Haskell codegen implementation, you can run the build in this module to see how those changes affect the generated Haskell code.

To build the test model and generate Haskell code:

```bash
gradle :client-codegen-test:build
```
