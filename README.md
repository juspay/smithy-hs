# <img alt="Smithy" src="https://github.com/smithy-lang/smithy/blob/main/docs/_static/smithy-anvil.svg?raw=true" width="32"> Smithy Haskell
[Smithy](https://smithy.io/2.0/index.html) code generators for [Haskell](https://www.haskell.org/).

> [!WARNING]
> This project is still in development and is not intended for use in production.

## Setup
This projects dev environment is setup with `nix`. To get started just run `nix develop` & you will get all the tooling required to build & test the project.

## Building
The `build` task runs `smithy-build` as part of the build process. So once the codegen plugin is compiled, it will automatically run the plugin & generate
code from a smithy model meant for testing. You can take a look at the `client-codegen-test` module.

## Testing
Run `cabal test` in `client-codegen-test/hs-it`, this will test the generated-client for the modeled service.

## Trying out the plugin
As of right now, the plugin hasn't been published anywhere. So if you wish to try it out with your own smithy model, you will have to publish it yourself.
Just run `gradle publish`, this will create a maven package to `client-codegen/build/m2`.
To refer to this plugin when using the command line see: https://smithy.io/2.0/guides/smithy-build-json.html#smithy-maven-repos-environment-variable
