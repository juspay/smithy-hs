{
  inputs = {
    utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ## In nixpkgs, `gradle` is a wrapped bin & resets the JAVA_HOME env.
        ## So to workaround it using a gradle property to point to the
        ## nix store jdk.
        ## FIXME Figure out a way to avoid this.
        gradle = (
          pkgs.writeShellScriptBin "gradle" "${pkgs.gradle_8}/bin/gradle -Porg.gradle.java.installations.paths=${pkgs.jdk17}/lib/openjdk \${@}"
        );
        hpkgs = pkgs.haskell.packages.ghc964;
        codegen-dir = ./client-codegen-test/output/source/haskell-client-codegen;
        haskell-client-codegen = hpkgs.callCabal2nix "haskell-client-codegen" codegen-dir { };
        hshell = hpkgs.developPackage {
          root = ./client-codegen-test/hs-it;
          source-overrides = {
            test-client-sdk = codegen-dir;
          };
          modifier = drv: pkgs.haskell.lib.addBuildTools drv (with hpkgs; [
            cabal-install
            ## FIXME HLS build is failing on darwin-arm.
            # haskell-language-server
          ]);
        };
      in
      {
        devShells.default = hshell.env.overrideAttrs (old:  {
          buildInputs = (old.buildInputs or []) ++ [
            gradle
            pkgs.jdk17
          ];
          shellHook = (old.shellHook or "") + ''
            export JAVA_HOME=${pkgs.jdk17}
          '';
        });
        checks = {
          client-codegen = haskell-client-codegen;
        };
      }
    );
}
