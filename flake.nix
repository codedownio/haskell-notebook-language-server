{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lsp-types = pkgs.haskell.packages.ghc8107.callPackage ./lsp-types.nix {};
      in
        rec {
          packages = rec {
            inherit (pkgs) cabal2nix;
            inherit lsp-types;

            default-810 = pkgs.haskell.packages.ghc810.callPackage ./default.nix { inherit lsp-types; };
            default-90 = pkgs.haskell.packages.ghc90.callPackage ./default.nix { inherit lsp-types; };
            default-92 = pkgs.haskell.packages.ghc92.callPackage ./default.nix { inherit lsp-types; };
            default-94 = pkgs.haskell.packages.ghc94.callPackage ./default.nix { inherit lsp-types; };
            default-96 = pkgs.haskell.packages.ghc96.callPackage ./default.nix { inherit lsp-types; };
          };

          defaultPackage = packages.default-90;

          nixpkgsPath = pkgs.path;
        }
    );
}
