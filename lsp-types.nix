{ mkDerivation, aeson, base, binary, containers, data-default
, deepseq, Diff, directory, dlist, exceptions, file-embed, filepath
, hashable, hspec, hspec-discover, lens, lib, mod, mtl, network-uri
, prettyprinter, QuickCheck, quickcheck-instances, regex, row-types
, safe, some, template-haskell, text, unordered-containers
, fetchFromGitHub, stdenv
}:
mkDerivation {
  pname = "lsp-types";
  version = "2.0.0.1";
  src = stdenv.mkDerivation {
    pname = "lsp-types-src";
    version = "2.0.0.1";

    src = fetchFromGitHub {
      owner = "haskell";
      repo = "lsp";
      rev = "e9d2cff6c41152a14a09c46e50c32f0d99070798";
      sha256 = "189518wh05w45j10wdv27h4dy2nbaf4dh7adh47kmjbw90cir3dn";
    };

    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;
    installPhase = "cp -r lsp-types $out";
    dontFixup = true;
  };

  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    aeson base binary containers data-default deepseq Diff dlist
    exceptions file-embed filepath hashable lens mod mtl network-uri
    row-types safe some template-haskell text unordered-containers
  ];
  executableHaskellDepends = [
    base containers directory filepath mtl prettyprinter regex text
  ];
  testHaskellDepends = [
    aeson base filepath hspec lens network-uri QuickCheck
    quickcheck-instances row-types text
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = lib.licenses.mit;
  mainProgram = "generator";
}
