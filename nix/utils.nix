{ pkgs, gitignore, system }:

{
  srcWithStackYaml = stackYaml: let
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

    mkdir -p $out
    cp ${hnls}/bin/haskell-notebook-language-server $out/$name

    cd $out
    chmod u+w "$name"

    # We don't need to strip here because we do it in the build with dontStrip=false
    # strip "$name"

    tar -czvf $name.tar.gz $name
  '';
}
