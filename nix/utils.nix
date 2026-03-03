{ pkgs, gitignore, system }:

{
  src = stackYaml: let
    baseSrc = pkgs.lib.cleanSourceWith {
      src = gitignore.lib.gitignoreSource ../.;
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

  packageForGitHub = ghcName: hnls: pkgs.runCommand "haskell-notebook-language-server-${hnls.version}" { nativeBuildInputs = [pkgs.binutils]; } ''
    name="haskell-notebook-language-server-${hnls.version}-${ghcName}-${system}"

    mkdir -p $out/$name/bin
    cp ${hnls}/bin/haskell-notebook-language-server $out/$name/bin/haskell-notebook-language-server

    chmod u+w "$out/$name/bin/haskell-notebook-language-server"

    cd $out
    tar -czvf $name.tar.gz $name
  '';

  packageForGitHubBundled = ghcName: hnls: let
    bundled = pkgs.callPackage ./package-bundled.nix {
      binaryDrv = hnls;
      binaryName = "haskell-notebook-language-server";
    };
  in pkgs.runCommand "haskell-notebook-language-server-${hnls.version}" {} ''
    name="haskell-notebook-language-server-${hnls.version}-${ghcName}-${system}"

    mkdir -p $out/$name
    cp -r ${bundled}/bin $out/$name/bin
    cp -r ${bundled}/lib $out/$name/lib

    cd $out
    tar -czvf $name.tar.gz $name
  '';
}
