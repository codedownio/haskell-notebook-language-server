final: prev: {
  haskell-nix = let
    shouldPatch = name: compiler: builtins.elem name [
      "ghc902"
      "ghc928"
      "ghc948"
      "ghc967"
      "ghc984"
      "ghc9102"
      "ghc9122"
    ];

    overrideCompiler = name: compiler: (compiler.override {
      enableNativeBignum = true;
    });
  in
    prev.lib.recursiveUpdate prev.haskell-nix {
      compiler = prev.lib.mapAttrs overrideCompiler (prev.lib.filterAttrs shouldPatch prev.haskell-nix.compiler);
    };
}
