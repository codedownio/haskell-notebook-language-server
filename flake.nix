{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lsp-types = pkgs.haskell.packages.ghc8107.callPackage ./lsp-types.nix {};
        default = pkgs.haskell.packages.ghc8107.callPackage ./default.nix {
          inherit lsp-types;
        };
      in
        rec {
          packages = rec {
            inherit (pkgs) cabal2nix;
            inherit lsp-types;
            inherit default;
          };

          defaultPackage = packages.default;

          nixpkgsPath = pkgs.path;
        }
    );
}
