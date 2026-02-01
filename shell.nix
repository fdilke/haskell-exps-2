{pkgs ? import <nixpkgs> {}}:
pkgs.haskellPackages.shellFor {
  packages = hpkgs: [
    (hpkgs.callPackage ./haskell-exps-2.nix {})
  ];
  nativeBuildInputs = with pkgs; [
    haskell-language-server
    cabal-install
  ];
  withHoogle = true;
  shellHook = "unset TEMP TMP TEMPDIR TMPDIR";
}
