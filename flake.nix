{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  # Pinned exactly: this floated on master, and the nixpkgs it dragged in took binutils from
  # 2.44 to 2.46, whose ld.gold can't link GHC's bootstrap haddock.
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/3d182e3e08cedf07c7bc56c11e3eb868ecf5f15d";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs";

  # GHC 9.14 needs a haskell.nix new enough to know ghc9141, and that one carries binutils 2.46,
  # so only the 9.14 outputs are built against it.
  inputs.haskellNixNew.url = "github:input-output-hk/haskell.nix/630f369f18e7c0637649e4ea5aad20efd7a23e66";
  inputs.nixpkgsNew.follows = "haskellNixNew/nixpkgs";

  inputs.linux-dynamic-bundler.url = "github:codedownio/linux-dynamic-bundler";

  inputs.haskellNixOld.url = "github:input-output-hk/haskell.nix/7fee6ed25386a600d6bcdded728a7d3d6ad7e15c";
  # inputs.nixpkgsOld.follows = "haskellNixOld/nixpkgs";
  inputs.nixpkgsOld.url = "github:NixOS/nixpkgs/e39f5c80e49a2abbf2b5631890476591072623fd";

  outputs = { self, flake-utils, gitignore, linux-dynamic-bundler, haskellNix, nixpkgs, haskellNixNew, nixpkgsNew, haskellNixOld, nixpkgsOld }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        overlaysFor = haskellNixToUse: [
          haskellNixToUse.overlay
          (import ./nix/enable-native-bignum-overlay.nix)
          (import ./nix/fix-ghc-pkgs-overlay.nix system)
        ];

        pkgs = import nixpkgs { inherit system; overlays = overlaysFor haskellNix; inherit (haskellNix) config; };
        # haskell.nix floors at GHC 9.6, so 9.4 builds against the older pin.
        pkgsOld = import nixpkgsOld { inherit system; overlays = overlaysFor haskellNixOld; inherit (haskellNixOld) config; };
        # The newer haskell.nix is needed only for ghc9141; pairing it with the older nixpkgs keeps
        # binutils at 2.44, whose gold links the static build (2.46's segfaults, and its musl
        # wrapper is prefix-only so -pgml g++ can't resolve).
        pkgsNew = import nixpkgs { inherit system; overlays = overlaysFor haskellNixNew ++ [(import ./nix/serial-bindist-overlay.nix system)]; inherit (haskellNixNew) config; };

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

        flakeStaticOld = compiler-nix-name: src: extraModules:
          (pkgsOld.pkgsCross.musl64.hixProject compiler-nix-name src ([baseModules] ++ extraModules ++ [(import ./nix/static-modules.nix { pkgs = pkgsOld; })])).flake {};

        flakeNew = compiler-nix-name: src: extraModules:
          (pkgsNew.hixProject compiler-nix-name src ([baseModules] ++ extraModules)).flake {};

        flakeStaticNew = compiler-nix-name: src: extraModules:
          (pkgsNew.pkgsCross.musl64.hixProject compiler-nix-name src ([baseModules] ++ extraModules ++ [(import ./nix/static-modules.nix { pkgs = pkgsNew; })])).flake {};

        utils = import ./nix/utils.nix { inherit pkgs gitignore system linux-dynamic-bundler; };

        allVersions = lib.mapAttrs (_: v: v.packages."haskell-notebook-language-server:exe:haskell-notebook-language-server") {
          ghc94 = flakeOld "ghc948" (utils.src "stack/stack-9.4.8.yaml") [];
          ghc96 = flake "ghc967" (utils.src "stack/stack-9.6.7.yaml") [];
          ghc98 = flake "ghc984" (utils.src "stack/stack-9.8.4.yaml") [];
          ghc910 = flake "ghc9102" (utils.src "stack/stack-9.10.2.yaml") [(import ./nix/os-string-module.nix)];
          ghc912 = flake "ghc9122" (utils.src "stack/stack-9.12.2.yaml") [(import ./nix/os-string-module.nix)];
          ghc914 = flakeNew "ghc9141" (utils.src "stack/stack-9.14.1.yaml") [(import ./nix/os-string-module.nix)];

          ghc94-static = flakeStaticOld "ghc948" (utils.src "stack/stack-9.4.8.yaml") [];
          ghc96-static = flakeStatic "ghc967" (utils.src "stack/stack-9.6.7.yaml") [];
          ghc98-static = flakeStatic "ghc984" (utils.src "stack/stack-9.8.4.yaml") [];
          ghc910-static = flakeStatic "ghc9102" (utils.src "stack/stack-9.10.2.yaml") [(import ./nix/os-string-module.nix)];
          ghc912-static = flakeStatic "ghc9122" (utils.src "stack/stack-9.12.2.yaml") [(import ./nix/os-string-module.nix)];
          ghc914-static = flakeStaticNew "ghc9141" (utils.src "stack/stack-9.14.1.yaml") [(import ./nix/os-string-module.nix)];
        };

        staticVersions = lib.mapAttrsToList (n: v: utils.packageForGitHub (lib.removeSuffix "-static" n) v)
                                            (lib.filterAttrs (n: v: lib.hasSuffix "-static" n) allVersions);

        dynamicVersions = lib.mapAttrsToList utils.packageForGitHub
                                             (lib.filterAttrs (n: v: !(lib.hasSuffix "-static" n)) allVersions);

        bundledVersions = lib.mapAttrsToList utils.packageForGitHubBundled
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

                pkgs.haskell.compiler.ghc9122
              ];
            };
          };

          packages = allVersions // rec {
            inherit (pkgs) cabal2nix;

            # The other proxies all name their static build "default"; match them so the
            # shared aliases can read the version the same way.
            default = allVersions.ghc912-static;

            all = pkgs.linkFarm "haskell-notebook-language-server-all" allVersions;

            githubArtifacts = with pkgs; symlinkJoin {
              name = "haskell-notebook-language-server-artifacts";
              paths =
                if pkgs.stdenv.isDarwin then dynamicVersions
                else if system == "aarch64-linux" then bundledVersions
                else staticVersions;
            };

            grandCombinedGithubArtifacts = pkgs.symlinkJoin {
              name = "haskell-notebook-language-server-grand-combined-artifacts";
              paths = [
                self.packages.x86_64-linux.githubArtifacts
                self.packages.aarch64-linux.githubArtifacts
                self.packages.x86_64-darwin.githubArtifacts
                self.packages.aarch64-darwin.githubArtifacts
              ];
            };

            gcroots = pkgs.writeText "haskell-notebook-language-server-gc-roots" ''
              ${githubArtifacts}
              ${pkgs.symlinkJoin {
                name = "my-project-deps";
                paths = lib.concatMap (v: v.buildInputs ++ v.nativeBuildInputs) (lib.attrValues allVersions);
              }}
            '';

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          } // allVersions;

          inherit flake;
        }
    );
}
