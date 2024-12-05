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
              overrideCompiler = name: compiler: (compiler.override {
                enableNativeBignum = true;
              });
            in
              prev.lib.recursiveUpdate prev.haskell-nix {
                compiler = prev.lib.mapAttrs overrideCompiler prev.haskell-nix.compiler;
              };
          })

          (import ./nix/fix-ghc-pkgs-overlay.nix system)
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        flake = compiler-nix-name: src: (pkgs.hixProject compiler-nix-name src [{
          # configureFlags = [
          #   ''--ghc-options="-with-rtsopts=-M12G"''
          # ];
        }]).flake {};

        flakeStatic = compiler-nix-name: src: (pkgs.pkgsCross.musl64.hixProject compiler-nix-name src [{
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.dontStrip = false;
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.enableShared = false;
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.configureFlags = [
            ''--ghc-options="-pgml g++ -optl=-fuse-ld=gold -optl-Wl,--allow-multiple-definition -optl-Wl,--whole-archive -optl-Wl,-Bstatic -optl-Wl,-Bdynamic -optl-Wl,--no-whole-archive"''
          ];
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.libs = [];
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.build-tools = [pkgs.pkgsCross.musl64.gcc];
        }]).flake {};

        srcWithStackYaml = stackYaml: let
          baseSrc = gitignore.lib.gitignoreSource ./.;
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

        packageForGitHub = hnls: ghcName: pkgs.runCommand "haskell-notebook-language-server-${hnls.version}" { nativeBuildInputs = [pkgs.binutils]; } ''
          name="haskell-notebook-language-server-${hnls.version}-${ghcName}-x86_64-linux"

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
            { name = "ghc810"; ghc = "ghc8107"; stackYaml = "stack/stack-8.10.7.yaml"; }
            { name = "ghc90"; ghc = "ghc902"; stackYaml = "stack/stack-9.0.2.yaml"; }
            { name = "ghc92"; ghc = "ghc928"; stackYaml = "stack/stack-9.2.8.yaml"; }
            { name = "ghc94"; ghc = "ghc948"; stackYaml = "stack/stack-9.4.8.yaml"; }
            { name = "ghc96"; ghc = "ghc966"; stackYaml = "stack/stack-9.6.6.yaml"; }
            { name = "ghc98"; ghc = "ghc983"; stackYaml = "stack/stack-9.8.2.yaml"; }
          ]
        );

      in
        {
          devShells = {
            default = pkgs.mkShell {
              NIX_PATH = "nixpkgs=${pkgs.path}";
            };
          };

          packages = {
            inherit (pkgs) cabal2nix;

            all = pkgs.linkFarm "haskell-notebook-language-server-all" allVersions;

            githubArtifacts = with pkgs; symlinkJoin {
              name = "haskell-notebook-language-server-artifacts";
              paths = lib.mapAttrsToList (n: v: packageForGitHub v (lib.removeSuffix "-static" n))
                                         (pkgs.lib.filterAttrs (n: v: pkgs.lib.hasSuffix "-static" n) allVersions);
            };

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          } // allVersions;

          inherit flake;
        }
    );
}
