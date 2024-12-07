{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/angerman/fix-install_name_tool";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        overlays = [
          haskellNix.overlay

          # Set enableNativeBignum flag on compiler
          (final: prev: {
            haskell-nix = let
              shouldPatch = name: compiler: builtins.elem name [
                "ghc902"
                "ghc928"
                "ghc948"
                "ghc966"
                "ghc982"
              ];

              overrideCompiler = name: compiler: (compiler.override {
                enableNativeBignum = true;
              });
            in
              prev.lib.recursiveUpdate prev.haskell-nix {
                compiler = prev.lib.mapAttrs overrideCompiler (prev.lib.filterAttrs shouldPatch prev.haskell-nix.compiler);
              };
          })

          (import ./nix/fix-ghc-pkgs-overlay.nix system)
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        baseModules = {
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.dontStrip = false;
        } // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.postInstall = ''
            ${builtins.readFile ./nix/fix-dylib.sh}

            fix_dylib "$out/bin/haskell-notebook-language-server" libiconv.2.dylib libiconv.dylib
            fix_dylib "$out/bin/haskell-notebook-language-server" libffi.8.dylib libffi.dylib
            fix_dylib "$out/bin/haskell-notebook-language-server" libncursesw.6.dylib libncurses.dylib
            check_no_nix_refs "$out/bin/haskell-notebook-language-server"

            strip "$out/bin/haskell-notebook-language-server"
          '';
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.configureFlags = let
            # Nixpkgs can't currently give us a cross-compiled x86_64-darwin libffi.a when we're building on aarch64-darwin.
            # So, we bundle one in the repo.
            # Tried to also detect if we're on aarch64-darwin, so it can work normally if the build machine is x86_64-darwin,
            # but that is deliberately difficult here (builtins.currentSystem is considered an "impure builtin".)
            libffi = if pkgs.stdenv.targetPlatform.system == "x86_64-darwin"
                     then "${./assets/libffi.a}"
                     else "${pkgs.pkgsStatic.libffi}/lib/libffi.a";
            in
              [
                ''--ghc-options="-optl-Wl,-dead_strip -optl-Wl,-dead_strip_dylibs -optl-Wl,-force_load,${libffi}"''
              ];
        };

        flake = compiler-nix-name: src: (pkgs.hixProject compiler-nix-name src [baseModules]).flake {};

        flakeStatic = compiler-nix-name: src: (pkgs.pkgsCross.musl64.hixProject compiler-nix-name src [baseModules {
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.enableShared = false;
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.configureFlags = [
            ''--ghc-options="-pgml g++ -optl=-fuse-ld=gold -optl-Wl,--allow-multiple-definition -optl-Wl,--whole-archive -optl-Wl,-Bstatic -optl-Wl,-Bdynamic -optl-Wl,--no-whole-archive"''
          ];
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.libs = [];
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.build-tools = [pkgs.pkgsCross.musl64.gcc];
        }]).flake {};

        srcWithStackYaml = stackYaml: let
          baseSrc = pkgs.lib.cleanSourceWith {
          src = gitignore.lib.gitignoreSource ./.;
          filter = name: type:
            !(baseNameOf name == "flake.nix");
          };
        in
          pkgs.runCommand "src-with-${stackYaml}" {} ''
            cp -r ${baseSrc} $out
            chmod u+w $out
            cd $out
            cp ${stackYaml} stack.yaml
            cp ${stackYaml}.lock stack.yaml.lock
            sed -i 's/\.\././g' stack.yaml
          '';

        exeAttr = "haskell-notebook-language-server:exe:haskell-notebook-language-server";

        packageForGitHub = ghcName: hnls: pkgs.runCommand "haskell-notebook-language-server-${hnls.version}" { nativeBuildInputs = [pkgs.binutils]; } ''
          name="haskell-notebook-language-server-${hnls.version}-${ghcName}-${system}"

          mkdir -p $out
          cp ${hnls}/bin/haskell-notebook-language-server $out/$name

          cd $out
          chmod u+w "$name"

          # We don't need to strip here because we do it in the build with dontStrip=false
          # strip "$name"

          tar -czvf $name.tar.gz $name
        '';

        allVersions = with pkgs.lib; listToAttrs (
          concatMap (info: [
            (nameValuePair info.name (flake info.ghc (srcWithStackYaml info.stackYaml)).packages.${exeAttr})
            (nameValuePair "${info.name}-static" (flakeStatic info.ghc (srcWithStackYaml info.stackYaml)).packages.${exeAttr})
          ]) [
            # { name = "ghc810"; ghc = "ghc8107"; stackYaml = "stack/stack-8.10.7.yaml"; }
            # { name = "ghc90"; ghc = "ghc902"; stackYaml = "stack/stack-9.0.2.yaml"; }
            { name = "ghc92"; ghc = "ghc928"; stackYaml = "stack/stack-9.2.8.yaml"; }
            { name = "ghc94"; ghc = "ghc948"; stackYaml = "stack/stack-9.4.8.yaml"; }
            { name = "ghc96"; ghc = "ghc966"; stackYaml = "stack/stack-9.6.6.yaml"; }
            { name = "ghc98"; ghc = "ghc982"; stackYaml = "stack/stack-9.8.2.yaml"; }
          ]
        );

        lib = pkgs.lib;

        staticVersions = lib.mapAttrsToList (n: v: packageForGitHub (lib.removeSuffix "-static" n) v)
                                            (pkgs.lib.filterAttrs (n: v: pkgs.lib.hasSuffix "-static" n) allVersions);

        dynamicVersions = lib.mapAttrsToList packageForGitHub
                                             (pkgs.lib.filterAttrs (n: v: !(pkgs.lib.hasSuffix "-static" n)) allVersions);

      in
        {
          devShells = {
            default = pkgs.mkShell {
              NIX_PATH = "nixpkgs=${pkgs.path}";
            };
          };

          packages = rec {
            inherit (pkgs) cabal2nix;

            all = pkgs.linkFarm "haskell-notebook-language-server-all" allVersions;

            githubArtifacts = with pkgs; symlinkJoin {
              name = "haskell-notebook-language-server-artifacts";
              paths = if pkgs.stdenv.isDarwin then dynamicVersions else staticVersions;
            };

            gcroots = pkgs.writeText "haskell-notebook-language-server-gc-roots" ''
              ${githubArtifacts}
              ${pkgs.symlinkJoin {
                name = "my-project-deps";
                paths =
                  allVersions.ghc98.buildInputs
                  ++ allVersions.ghc98.nativeBuildInputs
                  ++ allVersions.ghc96.buildInputs
                  ++ allVersions.ghc96.nativeBuildInputs
                  ++ allVersions.ghc94.buildInputs
                  ++ allVersions.ghc94.nativeBuildInputs
                  ++ allVersions.ghc92.buildInputs
                  ++ allVersions.ghc92.nativeBuildInputs
                ;
              }}
            '';

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          } // allVersions;

          inherit flake;
        }
    );
}
