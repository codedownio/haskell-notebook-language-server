{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, filepath, ghc, ghc-lib-parser, ghc-parser, ghc-paths, hpack
, ihaskell, lens, lib, lsp-types, monad-logger, mtl, network-uri
, optparse-applicative, QuickCheck, regex-base, regex-pcre-builtin
, safe, sandwich, string-interpolate, text, text-rope, unliftio
, unliftio-core, vector
}:
mkDerivation {
  pname = "haskell-notebook-language-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers ghc ghc-lib-parser ghc-parser ghc-paths ihaskell
    lsp-types monad-logger mtl regex-base regex-pcre-builtin
    string-interpolate text text-rope vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers filepath ghc
    ghc-lib-parser ghc-parser ghc-paths ihaskell lens lsp-types
    monad-logger mtl network-uri optparse-applicative QuickCheck
    regex-base regex-pcre-builtin safe sandwich string-interpolate text
    text-rope unliftio unliftio-core vector
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
