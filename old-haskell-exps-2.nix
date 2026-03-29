{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "haskell-exps-2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  doHaddock = false;
  description = "Sandbox";
  license = lib.licenses.bsd3;
  mainProgram = "haskell-exps";
}
