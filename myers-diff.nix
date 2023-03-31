{ mkDerivation, array, base, containers, criterion, deepseq
, exceptions, lib, primitive, QuickCheck, quickcheck-instances
, random, sandwich, sandwich-quickcheck, string-interpolate, text
, text-rope, vector, weigh, fetchFromGitHub
}:
mkDerivation {
  pname = "myers-diff";
  version = "0.2.0.0";
  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "myers-diff";
    rev = "cf0ecdd3cb9f15b97cf2b74e221c2f8b62ee37b9";
    sha256 = "sha256:0npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers exceptions primitive text vector
  ];
  executableHaskellDepends = [
    base containers deepseq exceptions primitive QuickCheck
    quickcheck-instances string-interpolate text text-rope vector
  ];
  testHaskellDepends = [
    base containers deepseq exceptions primitive QuickCheck
    quickcheck-instances sandwich sandwich-quickcheck
    string-interpolate text text-rope vector
  ];
  benchmarkHaskellDepends = [
    array base containers criterion deepseq exceptions primitive
    QuickCheck quickcheck-instances random string-interpolate text
    text-rope vector weigh
  ];
  homepage = "https://github.com/codedownio/myers-diff#readme";
  license = lib.licenses.bsd3;
  mainProgram = "myers-diff";
}
