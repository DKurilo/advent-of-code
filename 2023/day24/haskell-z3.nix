{
  pkgs, z3
}:
pkgs.haskellPackages.callPackage (
  { mkDerivation, base, containers, fetchgit, hspec, lib, QuickCheck
  , transformers
  }:
  mkDerivation {
    pname = "haskell-z3";
    version = "411.0.1";
    src = fetchgit {
      url = "https://github.com/IagoAbal/haskell-z3.git";
      sha256 = "11pabc9j52754adfdza8dcs710pcj3dfh5dnq045j952026255mj";
      rev = "b77a17e5eeb7db82656bcbcd66c6e952207e69ca";
      fetchSubmodules = true;
    };
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [ base containers transformers ];
    librarySystemDepends = [ z3 ];
    testHaskellDepends = [ base hspec QuickCheck ];
    homepage = "https://github.com/IagoAbal/haskell-z3";
    description = "Bindings for the Z3 Theorem Prover";
    license = lib.licenses.bsd3;
    doCheck = false;
  }) { }
