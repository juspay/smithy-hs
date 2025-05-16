{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.developPackage {
  name = "test-client-sdk";
  root = ./output/source/haskell-client-codegen;
  modifier = drv:
    pkgs.haskell.lib.overrideCabal drv (oldAttrs: {
      buildTools = (oldAttrs.buildTools or []) ++ [
        pkgs.haskellPackages.cabal-install
      ];
    });
}
