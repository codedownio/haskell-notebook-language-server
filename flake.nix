{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/master";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs";

  inputs.haskellNixOld.url = "github:input-output-hk/haskell.nix/7fee6ed25386a600d6bcdded728a7d3d6ad7e15c";
  # inputs.nixpkgsOld.follows = "haskellNixOld/nixpkgs";
  inputs.nixpkgsOld.url = "github:NixOS/nixpkgs/e39f5c80e49a2abbf2b5631890476591072623fd";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs, haskellNixOld, nixpkgsOld }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        overlays = [
          haskellNix.overlay
          (import ./nix/enable-native-bignum-overlay.nix)
          (import ./nix/fix-ghc-pkgs-overlay.nix system)
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        pkgsOld = import nixpkgsOld { inherit system overlays; inherit (haskellNixOld) config; };

        inherit (pkgs) lib;
        isDarwin = pkgs.stdenv.hostPlatform.isDarwin;

        baseModules = {
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.dontStrip = false;
        } // pkgs.lib.optionalAttrs isDarwin (import ./nix/darwin-modules.nix { inherit pkgs; });

        flake = compiler-nix-name: src: extraModules:
          (pkgs.hixProject compiler-nix-name src ([baseModules] ++ extraModules)).flake {};

        flakeOld = compiler-nix-name: src: extraModules:
          (pkgsOld.hixProject compiler-nix-name src ([baseModules] ++ extraModules)).flake {};

        flakeStatic = compiler-nix-name: src: extraModules:
          (pkgs.pkgsCross.musl64.hixProject compiler-nix-name src ([baseModules] ++ extraModules ++ [(import ./nix/static-modules.nix { inherit pkgs; })])).flake {};

        utils = import ./nix/utils.nix { inherit pkgs gitignore system; };

        allVersions = lib.mapAttrs (_: v: v.packages."haskell-notebook-language-server:exe:haskell-notebook-language-server") {
          ghc92 = (if isDarwin then flakeOld else flake) "ghc928" (utils.src "stack/stack-9.2.8.yaml") [];
          ghc94 = (if isDarwin then flakeOld else flake) "ghc948" (utils.src "stack/stack-9.4.8.yaml") [];
          ghc96 = flake "ghc967" (utils.src "stack/stack-9.6.7.yaml") [];
          ghc98 = flake "ghc984" (utils.src "stack/stack-9.8.4.yaml") [];
          ghc910 = flake "ghc9102" (utils.src "stack/stack-9.10.2.yaml") [(import ./nix/os-string-module.nix)];
          ghc912 = flake "ghc9122" (utils.src "stack/stack-9.12.2.yaml") [(import ./nix/os-string-module.nix)];

          ghc92-static = flakeStatic "ghc928" (utils.src "stack/stack-9.2.8.yaml") [];
          ghc94-static = flakeStatic "ghc948" (utils.src "stack/stack-9.4.8.yaml") [];
          ghc96-static = flakeStatic "ghc967" (utils.src "stack/stack-9.6.7.yaml") [];
          ghc98-static = flakeStatic "ghc984" (utils.src "stack/stack-9.8.4.yaml") [];
          ghc910-static = flakeStatic "ghc9102" (utils.src "stack/stack-9.10.2.yaml") [(import ./nix/os-string-module.nix)];
          ghc912-static = flakeStatic "ghc9122" (utils.src "stack/stack-9.12.2.yaml") [(import ./nix/os-string-module.nix)];
        };

        staticVersions = lib.mapAttrsToList (n: v: utils.packageForGitHub (lib.removeSuffix "-static" n) v)
                                            (lib.filterAttrs (n: v: lib.hasSuffix "-static" n) allVersions);

        dynamicVersions = lib.mapAttrsToList utils.packageForGitHub
                                             (lib.filterAttrs (n: v: !(lib.hasSuffix "-static" n)) allVersions);

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

          packages = allVersions // rec {
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
