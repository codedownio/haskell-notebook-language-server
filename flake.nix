{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/master";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        overlays = [
          haskellNix.overlay
          (import ./nix/enable-native-bignum-overlay.nix)
          (import ./nix/fix-ghc-pkgs-overlay.nix system)
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        baseModules = {
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.dontStrip = false;
        } // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin (import ./nix/darwin-modules.nix { inherit pkgs; });

        flake = compiler-nix-name: src: extraModules:
          (pkgs.hixProject compiler-nix-name src ([baseModules] ++ extraModules)).flake {};

        flakeStatic = compiler-nix-name: src: extraModules:
          (pkgs.pkgsCross.musl64.hixProject compiler-nix-name src ([baseModules] ++ extraModules ++ [(import ./nix/static-modules.nix { inherit pkgs; })])).flake {};

        exeAttr = "haskell-notebook-language-server:exe:haskell-notebook-language-server";

        utils = import ./nix/utils.nix { inherit pkgs gitignore system; };

        allVersions = with pkgs.lib; listToAttrs (
          concatMap (info: [
            (nameValuePair info.name (flake info.ghc (utils.srcWithStackYaml info.stackYaml) info.extraModules).packages.${exeAttr})
            (nameValuePair "${info.name}-static" (flakeStatic info.ghc (utils.srcWithStackYaml info.stackYaml) info.extraModules).packages.${exeAttr})
          ]) [
            # { name = "ghc810"; ghc = "ghc8107"; stackYaml = "stack/stack-8.10.7.yaml"; }
            # { name = "ghc90"; ghc = "ghc902"; stackYaml = "stack/stack-9.0.2.yaml"; }
            { name = "ghc92"; ghc = "ghc928"; stackYaml = "stack/stack-9.2.8.yaml"; extraModules = []; }
            { name = "ghc94"; ghc = "ghc948"; stackYaml = "stack/stack-9.4.8.yaml"; extraModules = []; }
            { name = "ghc96"; ghc = "ghc967"; stackYaml = "stack/stack-9.6.7.yaml"; extraModules = []; }
            { name = "ghc98"; ghc = "ghc984"; stackYaml = "stack/stack-9.8.4.yaml"; extraModules = []; }
            { name = "ghc910"; ghc = "ghc9102"; stackYaml = "stack/stack-9.10.2.yaml"; extraModules = [(import ./nix/os-string-module.nix)]; }
            { name = "ghc912"; ghc = "ghc9122"; stackYaml = "stack/stack-9.12.2.yaml"; extraModules = [(import ./nix/os-string-module.nix)]; }
          ]
        );

        lib = pkgs.lib;

        staticVersions = lib.mapAttrsToList (n: v: utils.packageForGitHub (lib.removeSuffix "-static" n) v)
                                            (pkgs.lib.filterAttrs (n: v: pkgs.lib.hasSuffix "-static" n) allVersions);

        dynamicVersions = lib.mapAttrsToList utils.packageForGitHub
                                             (pkgs.lib.filterAttrs (n: v: !(pkgs.lib.hasSuffix "-static" n)) allVersions);

      in
        {
          devShells = {
            default = pkgs.mkShell {
              NIX_PATH = "nixpkgs=${pkgs.path}";

              buildInputs = with pkgs; [
                libsodium # Needed by zeromq
                pcre
                pkg-config
                zeromq
                zlib
              ];
            };
          };

          packages = rec {
            inherit (pkgs) cabal2nix;

            all = pkgs.linkFarm "haskell-notebook-language-server-all" allVersions;

            githubArtifacts = with pkgs; symlinkJoin {
              name = "haskell-notebook-language-server-artifacts";
              paths = if pkgs.stdenv.isDarwin then dynamicVersions else staticVersions;
            };

            grandCombinedGithubArtifacts = pkgs.symlinkJoin {
              name = "haskell-notebook-language-server-grand-combined-artifacts";
              paths = [
                self.packages.x86_64-linux.githubArtifacts
                self.packages.x86_64-darwin.githubArtifacts
                self.packages.aarch64-darwin.githubArtifacts
              ];
            };

            inherit (allVersions) ghc92 ghc94 ghc96 ghc98 ghc910 ghc912;

            gcroots = pkgs.writeText "haskell-notebook-language-server-gc-roots" ''
              ${githubArtifacts}
              ${pkgs.symlinkJoin {
                name = "my-project-deps";
                paths =
                  allVersions.ghc912.buildInputs ++ allVersions.ghc912.nativeBuildInputs
                  ++ allVersions.ghc910.buildInputs ++ allVersions.ghc910.nativeBuildInputs
                  ++ allVersions.ghc98.buildInputs ++ allVersions.ghc98.nativeBuildInputs
                  ++ allVersions.ghc96.buildInputs ++ allVersions.ghc96.nativeBuildInputs
                  ++ allVersions.ghc94.buildInputs ++ allVersions.ghc94.nativeBuildInputs
                  ++ allVersions.ghc92.buildInputs ++ allVersions.ghc92.nativeBuildInputs
                ;
              }}
            '';

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          } // allVersions;

          inherit flake;
        }
    );
}
