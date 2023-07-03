{ mkDerivation, aeson, base, bytestring, containers, exceptions
, filepath, ghc, ghc-parser, ghc-paths, ihaskell, lens
, lens-regex-pcre, lib, lsp-types, monad-logger, mtl, myers-diff
, network-uri, optparse-applicative, pcre-light, process
, QuickCheck, regex-base, regex-pcre-builtin, retry, row-types
, safe, sandwich, sandwich-quickcheck, string-interpolate, text
, text-rope, time, unix, unliftio, unliftio-core, vector
}:
mkDerivation {
  pname = "haskell-notebook-language-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers filepath ghc ghc-parser ghc-paths ihaskell
    lens lens-regex-pcre lsp-types monad-logger mtl myers-diff
    network-uri pcre-light regex-base regex-pcre-builtin row-types safe
    string-interpolate text text-rope time unliftio unliftio-core
    vector
  ];
  executableHaskellDepends = [
    aeson base bytestring ghc ghc-paths ihaskell lens lsp-types
    monad-logger mtl optparse-applicative process retry safe
    string-interpolate text unix unliftio unliftio-core
  ];
  testHaskellDepends = [
    base containers exceptions lsp-types monad-logger QuickCheck
    row-types sandwich sandwich-quickcheck string-interpolate text
    text-rope unliftio
  ];
  doCheck = false;
  license = lib.licenses.bsd3;
  mainProgram = "haskell-notebook-language-server";
}
