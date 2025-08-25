system:

final: prev: {
  hixProject = compiler-nix-name: src: extraModules:
    final.haskell-nix.hix.project {
      inherit src;
      evalSystem = system;
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
            "semaphore-compat"
            "text"
            "time"
            "transformers"
            "unix"
            "xhtml"
            "terminfo"

            "os-string"
            "file-io"
            "ghc-internal"
          ];
        }
      ] ++ extraModules;
    };
}
