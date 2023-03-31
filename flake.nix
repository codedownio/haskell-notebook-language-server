{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ce6aa13369b667ac2542593170993504932eb836";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        default = pkgs.haskell.packages.ghc8107.callPackage ./default.nix {};
        myersDiff = pkgs.haskell.packages.ghc8107.callPackage ./myers-diff.nix {};
      in
        rec {
          packages = rec {
            inherit default myersDiff;
            inherit (pkgs) cabal2nix;
          };

          defaultPackage = packages.default;

          nixpkgsPath = pkgs.path;
        }
    );
}
