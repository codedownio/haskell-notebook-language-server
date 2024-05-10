{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [
          haskellNix.overlay
          (import ./nix/fix-ghc-pkgs-overlay.nix)
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        # lsp-types = pkgs.haskell.packages.ghc8107.callPackage ./lsp-types.nix {};

        flake = compiler-nix-name: src: (pkgs.hixProject compiler-nix-name src []).flake {};

        staticModules = [{
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.dontStrip = false;
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.enableShared = false;
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.configureFlags = [
            ''--ghc-options="-pgml g++ -optl=-fuse-ld=gold -optl-Wl,--allow-multiple-definition -optl-Wl,--whole-archive -optl-Wl,-Bstatic -optl-Wl,-Bdynamic -optl-Wl,--no-whole-archive"''
          ];
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.libs = [];
          packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.build-tools = [pkgs.pkgsCross.musl64.gcc];
        }];

        flakeStatic = compiler-nix-name: src: (pkgs.pkgsCross.musl64.hixProject compiler-nix-name src staticModules).flake {};

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
          # strip "$name"

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
            { name = "ghc948"; stackYaml = "stack/stack-9.4.8.yaml"; }
            { name = "ghc964"; stackYaml = "stack/stack-9.6.4.yaml"; }
            { name = "ghc982"; stackYaml = "stack/stack-9.8.2.yaml"; }
          ]
        );

      in
        rec {
          packages = rec {
            inherit (pkgs) cabal2nix;
            # inherit lsp-types;

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
