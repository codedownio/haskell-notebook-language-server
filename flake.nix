{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject = compiler-nix-name: src:
              final.haskell-nix.hix.project {
                inherit src;
                evalSystem = "x86_64-linux";
                inherit compiler-nix-name;
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        # lsp-types = pkgs.haskell.packages.ghc8107.callPackage ./lsp-types.nix {};

        flake = compiler-nix-name: src: (pkgs.hixProject compiler-nix-name src).flake {};
        flakeStatic = compiler-nix-name: src: (pkgs.pkgsCross.musl64.hixProject compiler-nix-name src).flake {};

        srcWithStackYaml = stackYaml: let
          baseSrc = gitignore.lib.gitignoreSource ./.;
        in
          pkgs.runCommand "src-with-${stackYaml}" {} ''
            cp -r ${baseSrc} $out
            chmod u+w $out
            cd $out
            mv ${stackYaml} stack.yaml
            rm stack-*.yaml*
          '';

        exeAttr = "haskell-notebook-language-server:exe:haskell-notebook-language-server";

        allVersions = with pkgs.lib; (
          concatMap (info: [
            (nameValuePair info.name (flake info.name (srcWithStackYaml info.stackYaml)).packages.${exeAttr})
            (nameValuePair "${info.name}-static" (flakeStatic info.name (srcWithStackYaml info.stackYaml)).packages.${exeAttr})
          ]) [
            { name = "ghc8107"; stackYaml = "stack-8.10.7.yaml"; }
            { name = "ghc902"; stackYaml = "stack-9.0.2.yaml"; }
            { name = "ghc928"; stackYaml = "stack-9.2.8.yaml"; }
            { name = "ghc945"; stackYaml = "stack-9.4.5.yaml"; }
            # { name = "ghc962"; stackYaml = "stack-9.6.2.yaml"; }
          ]
        );

      in
        rec {
          packages = rec {
            inherit (pkgs) cabal2nix;
            # inherit lsp-types;

            all = with pkgs.lib; pkgs.linkFarm "haskell-notebook-language-server-all" (
              map (x: {
                inherit (x) name;
                path = x.value;
              }) allVersions
            );
          } // pkgs.lib.listToAttrs allVersions;

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
