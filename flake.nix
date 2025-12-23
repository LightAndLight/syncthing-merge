{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    hdeps = {
      url = "github:LightAndLight/hdeps";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, hdeps }:
    {
      nixosModules.default = import ./nix/nixos-module.nix;
      overlays.default = self: super: {
        syncthing-merge = self.haskellPackages.callPackage ./syncthing-merge {};
      };
    } //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          shellHook = ''
            export PROJECT_ROOT=$(git rev-parse --show-toplevel)
          '';

          buildInputs = with pkgs; [
            haskellPackages.ghc
            cabal-install
            haskell-language-server

            just
            haskellPackages.fourmolu
            haskellPackages.implicit-hie
            fd
            cabal2nix
            hdeps.packages.${system}.default

            zlib
          ];
        };
      }
    );
}
