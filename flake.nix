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
                modules = [
                  {
                    reinstallableLibGhc = false;
                    nonReinstallablePkgs = [
                      "rts"
                      "ghc-heap"
                      "ghc-prim"
                      "integer-gmp"
                      "integer-simple"
                      "base"
                      "deepseq"
                      "array"
                      "ghc-boot-th"
                      "pretty"
                      "template-haskell"
                      "ghcjs-prim"
                      "ghcjs-th"
                      "ghc-bignum"
                      "exceptions"
                      "stm"
                      "ghc-boot"
                      "ghc"
                      "Cabal"
                      "Win32"
                      "array"
                      "binary"
                      "bytestring"
                      "containers"
                      "directory"
                      "filepath"
                      "ghc-boot"
                      "ghc-compact"
                      "ghc-prim"
                      "hpc"
                      "mtl"
                      "parsec"
                      "process"
                      "text"
                      "time"
                      "transformers"
                      "unix"
                      "xhtml"
                      "terminfo"
                    ];
                  }
                ];
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
            cp ${stackYaml} stack.yaml
            cp ${stackYaml}.lock stack.yaml.lock
          '';

        exeAttr = "haskell-notebook-language-server:exe:haskell-notebook-language-server";

        packageForGitHub = hnls: ghcName: pkgs.runCommand "haskell-notebook-language-server-${hnls.version}" { nativeBuildInputs = [pkgs.binutils]; } ''
          name="haskell-notebook-language-server-${hnls.version}-${ghcName}-x86_64-linux"

          mkdir -p $out
          cp ${hnls}/bin/haskell-notebook-language-server $out/$name

          cd $out
          chmod u+w "$name"
          strip "$name"

          tar -czvf $name.tar.gz $name
        '';

        allVersions = with pkgs.lib; listToAttrs (
          concatMap (info: [
            (nameValuePair info.name (flake info.name (srcWithStackYaml info.stackYaml)).packages.${exeAttr})
            (nameValuePair "${info.name}-static" (flakeStatic info.name (srcWithStackYaml info.stackYaml)).packages.${exeAttr})
          ]) [
            { name = "ghc8107"; stackYaml = "stack/stack-8.10.7.yaml"; }
            { name = "ghc902"; stackYaml = "stack/stack-9.0.2.yaml"; }
            { name = "ghc928"; stackYaml = "stack/stack-9.2.8.yaml"; }
            { name = "ghc947"; stackYaml = "stack/stack-9.4.7.yaml"; }
            { name = "ghc963"; stackYaml = "stack/stack-9.6.3.yaml"; }
          ]
        );

      in
        rec {
          packages = rec {
            inherit (pkgs) cabal2nix;
            # inherit lsp-types;

            inherit allVersions;
            staticVersions = pkgs.lib.filterAttrs (n: v: pkgs.lib.hasSuffix "-static" n) allVersions;
            all = pkgs.linkFarm "haskell-notebook-language-server-all" allVersions;

            githubArtifacts = with pkgs; symlinkJoin {
              name = "haskell-notebook-language-server-artifacts";
              paths = lib.mapAttrsToList (n: v: packageForGitHub v (lib.removeSuffix "-static" n)) staticVersions;
            };
          } // allVersions;

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
