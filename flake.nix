{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  outputs = { self, flake-utils, haskellNix, nixpkgs }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject = compiler-nix-name:
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "x86_64-linux";
                inherit compiler-nix-name;
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        # lsp-types = pkgs.haskell.packages.ghc8107.callPackage ./lsp-types.nix {};

        flake = compiler-nix-name: (pkgs.hixProject compiler-nix-name).flake {};
        flakeStatic = compiler-nix-name: (pkgs.pkgsCross.musl64.hixProject compiler-nix-name).flake {};
      in
        rec {
          packages = rec {
            inherit (pkgs) cabal2nix;
            # inherit lsp-types;

            ghc902 = (flake "ghc902").packages."haskell-notebook-language-server:exe:haskell-notebook-language-server";
            ghc902-static = (flakeStatic "ghc902").packages."haskell-notebook-language-server:exe:haskell-notebook-language-server";
          };

          inherit flake;

          nixpkgsPath = pkgs.path;
        }
    );

  # nixConfig = {
  #   # This sets the flake to use the IOG nix cache.
  #   # Nix should ask for permission before using it,
  #   # but remove it here if you do not want it to.
  #   extra-substituters = ["https://cache.iog.io"];
  #   extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  #   allow-import-from-derivation = "true";
  # };
}
