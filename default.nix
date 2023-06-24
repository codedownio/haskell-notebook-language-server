{ mkDerivation, aeson, base, bytestring, containers, exceptions
, filepath, ghc, ghc-parser, ghc-paths, hpack, ihaskell, lens
, lens-regex-pcre, lib, lsp-types, monad-logger, mtl, myers-diff
, network-uri, optparse-applicative, pcre-light, process
, QuickCheck, regex-base, regex-pcre-builtin, retry, safe, sandwich
, sandwich-quickcheck, string-interpolate, text, text-rope, time
, unix, unliftio, unliftio-core, vector
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
    network-uri pcre-light regex-base regex-pcre-builtin safe
    string-interpolate text text-rope time unliftio unliftio-core
    vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring ghc ghc-paths ihaskell lens lsp-types
    monad-logger mtl optparse-applicative process retry safe
    string-interpolate text unix unliftio unliftio-core
  ];
  testHaskellDepends = [
    base containers exceptions lsp-types monad-logger QuickCheck
    sandwich sandwich-quickcheck string-interpolate text text-rope
    unliftio
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
