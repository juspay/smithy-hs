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
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            gradle
            pkgs.jdk17
          ];
          shellHook = ''
            export JAVA_HOME=${pkgs.jdk17}
          '';
        };
        checks = {
          client-codegen = haskell-client-codegen;
        };
      }
    );
}
