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
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            ## In nixpkgs, `gradle` is a wrapped bin & resets the JAVA_HOME env.
            ## So to workaround it I've used a gradle property to point to the
            ## nix store jdk.
            (writeShellScriptBin "gradle" "${pkgs.gradle_8}/bin/gradle -Porg.gradle.java.installations.paths=${pkgs.jdk17}/lib/openjdk \${@}")
            jdk17
          ];
          shellHook = ''
            export JAVA_HOME=${pkgs.jdk17}
          '';
        };
      }
    );
}
