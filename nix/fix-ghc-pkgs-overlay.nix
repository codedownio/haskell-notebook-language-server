system:

let
  ghc-libs = import ./ghc-libs.nix;

in

final: prev: {
  hixProject = compiler-nix-name: src: extraModules:
    final.haskell-nix.hix.project {
      inherit src;
      evalSystem = system;
      inherit compiler-nix-name;
      modules = [
        {
          reinstallableLibGhc = false;
          nonReinstallablePkgs = ghc-libs.${compiler-nix-name} ++ [
            "rts"
            "ghc-bignum"
          ] ++ prev.lib.optionals (compiler-nix-name == "ghc9102") [
            # "file-io"
            "os-string"
            "semaphore-compat"

            "ghc-internal"
          ];

          # nonReinstallablePkgs = [
          #   "rts" # Need
          #   "ghc-heap"
          #   "ghc-prim"
          #   "integer-gmp"
          #   # "integer-simple"
          #   "base"
          #   "deepseq"
          #   "array"
          #   "ghc-boot-th"
          #   "pretty"
          #   "template-haskell"
          #   "ghc-bignum" # Need
          #   "exceptions"
          #   "stm"
          #   "ghc-boot"
          #   "ghc"
          #   "Cabal"
          #   "Win32"
          #   "array"
          #   "binary"
          #   "bytestring"
          #   "containers"
          #   "directory"
          #   "filepath"
          #   "ghc-boot"
          #   "ghc-compact"
          #   "ghc-prim"
          #   "hpc"
          #   "mtl"
          #   "parsec"
          #   "process"
          #   "semaphore-compat"
          #   "text"
          #   "time"
          #   "transformers"
          #   "unix"
          #   "xhtml"
          #   "terminfo"

          #   "os-string"
          #   "file-io"
          #   "ghc-internal"
          # ];
        }
      ] ++ extraModules;
    };
}
