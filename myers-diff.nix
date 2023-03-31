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
    rev = "b01ccb5bbeb479ebfde817cc5ac3db8f8e2dae23";
    sha256 = "0vqi8ylyj48p4dzc65da18z5sv4w1fslq6bm8km86cg0mdxg3wpx";
  };
  doCheck = false;
  libraryHaskellDepends = [
    base containers exceptions primitive text vector
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
}
