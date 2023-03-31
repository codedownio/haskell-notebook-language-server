{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ce6aa13369b667ac2542593170993504932eb836";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        myers-diff = pkgs.haskell.packages.ghc8107.callPackage ./myers-diff.nix {};
        default = pkgs.haskell.packages.ghc8107.callPackage ./default.nix {
          inherit myers-diff;
        };
      in
        rec {
          packages = rec {
            inherit default myers-diff;
            inherit (pkgs) cabal2nix;
          };

          defaultPackage = packages.default;

          nixpkgsPath = pkgs.path;
        }
    );
}
