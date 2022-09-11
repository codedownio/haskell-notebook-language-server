{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, filepath, ghc, ghc-parser, ghc-paths, hpack, ihaskell, lens
, lens-regex-pcre, lib, lsp-types, monad-logger, mtl, network-uri
, optparse-applicative, pcre-light, process, regex-base
, regex-pcre-builtin, retry, safe, sandwich, string-interpolate
, text, text-rope, unix, unliftio, unliftio-core, vector
}:
mkDerivation {
  pname = "haskell-notebook-language-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers filepath ghc ghc-parser
    ghc-paths ihaskell lens lens-regex-pcre lsp-types monad-logger mtl
    network-uri optparse-applicative pcre-light process regex-base
    regex-pcre-builtin retry safe string-interpolate text text-rope
    unix unliftio unliftio-core vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers filepath ghc ghc-parser
    ghc-paths ihaskell lens lens-regex-pcre lsp-types monad-logger mtl
    network-uri optparse-applicative pcre-light process regex-base
    regex-pcre-builtin retry safe sandwich string-interpolate text
    text-rope unix unliftio unliftio-core vector
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
