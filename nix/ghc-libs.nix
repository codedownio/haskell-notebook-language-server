
# A bit of JS to scrape these from a GHC release notes page like
# https://downloads.haskell.org/ghc/latest/docs/users_guide/9.12.2-notes.html
#
# items = []; document.querySelectorAll("td:first-child").forEach((x) => items.push(x.innerText)); items

{
  "ghc902" = [
    "ghc" "Cabal" "Win32" "array" "base" "binary" "bytestring" "containers" "deepseq" "directory" "exceptions" "filepath" "ghc-boot-th" "ghc-boot" "ghc-compact" "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp" "libiserv" "mtl" "parsec" "pretty" "process" "stm" "template-haskell" "terminfo" "text" "time" "transformers" "unix" "xhtml"
  ];

  "ghc928" = [
    "ghc" "Cabal" "Win32" "array" "base" "binary" "bytestring" "containers" "deepseq" "directory" "exceptions" "filepath" "ghc-boot-th" "ghc-boot" "ghc-compact" "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp" "libiserv" "mtl" "parsec" "pretty" "process" "stm" "template-haskell" "terminfo" "text" "time" "transformers" "unix" "xhtml"
  ];

  "ghc948" = [
    "ghc" "Cabal-syntax" "Cabal" "Win32" "array" "base" "binary" "bytestring" "containers" "deepseq" "directory" "exceptions" "filepath" "ghc-boot-th" "ghc-boot" "ghc-compact" "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp" "libiserv" "mtl" "parsec" "pretty" "process" "stm" "template-haskell" "terminfo" "text" "time" "transformers" "unix" "xhtml"
  ];

  "ghc967" = [
    "ghc" "Cabal-syntax" "Cabal" "Win32" "array" "base" "binary" "bytestring" "containers" "deepseq" "directory" "exceptions" "filepath" "ghc-boot-th" "ghc-boot" "ghc-compact" "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp" "libiserv" "mtl" "parsec" "pretty" "process" "stm" "template-haskell" "terminfo" "text" "time" "transformers" "unix" "xhtml"
  ];

  "ghc984" = [
    "ghc" "Cabal-syntax" "Cabal" "Win32" "array" "base" "binary" "bytestring" "containers" "deepseq" "directory" "exceptions" "filepath" "ghc-boot-th" "ghc-boot" "ghc-compact" "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp" "mtl" "parsec" "pretty" "process" "semaphore-compat" "stm" "template-haskell" "terminfo" "text" "time" "transformers" "unix" "xhtml"
  ];

  "ghc9102" = [
    "ghc" "Cabal-syntax" "Cabal" "Win32" "array" "base" "binary" "bytestring" "containers" "deepseq" "directory" "exceptions" "filepath" "ghc-boot-th" "ghc-boot" "ghc-compact" "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp" "mtl" "parsec" "pretty" "process" "stm" "template-haskell" "terminfo" "text" "time" "transformers" "unix" "xhtml"
  ];

  "ghc9122" = [
    "ghc" "Cabal-syntax" "Cabal" "Win32" "array" "base" "binary" "bytestring" "containers" "deepseq" "directory" "exceptions" "file-io" "filepath" "ghc-boot-th" "ghc-boot" "ghc-compact" "ghc-experimental" "ghc-heap" "ghc-internal" "ghc-platform" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp" "mtl" "os-string" "parsec" "pretty" "process" "semaphore-compat" "stm" "template-haskell" "terminfo" "text" "time" "transformers" "unix" "xhtml" "haddock-api" "haddock-library"
  ];
}
