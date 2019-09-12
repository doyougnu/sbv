{ mkDerivation, array, async, base, bytestring, containers
, crackNum, criterion, deepseq, directory, doctest, filepath
, generic-deriving, ghc, Glob, hlint, mtl, pretty, process
, QuickCheck, random, stdenv, syb, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, template-haskell, time, transformers, z3
}:
mkDerivation {
  pname = "sbv";
  version = "8.4.5";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array async base containers crackNum deepseq directory filepath
    generic-deriving ghc mtl pretty process QuickCheck random syb
    template-haskell time transformers
  ];
  testHaskellDepends = [
    base bytestring containers crackNum directory doctest filepath Glob
    hlint mtl QuickCheck random syb tasty tasty-golden tasty-hunit
    tasty-quickcheck template-haskell
  ];
  testSystemDepends = [ z3 ];
  benchmarkHaskellDepends = [
    base containers crackNum criterion directory filepath mtl random
    syb
  ];
  homepage = "http://leventerkok.github.com/sbv/";
  description = "SMT Based Verification: Symbolic Haskell theorem prover using SMT solving";
  license = stdenv.lib.licenses.bsd3;
}
