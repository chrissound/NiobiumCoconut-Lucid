{ sources ? import ./nix/sources.nix
, compiler ? "ghc865"
, niobiumcoconut' ? (import (builtins.fetchGit {
          url = "https://github.com/chrissound/NiobiumCoconut.git";
          rev = "95933330dffd8e80dba796ae6eebbd0e93b29e63";
        }) )
, actiniumbravohoneydew' ? (import
        (builtins.fetchGit {
          url = "https://github.com/chrissound/ActiniumBravoHoneydew.git";
          rev = "46913901896f7488553f0560c22b20f7631d3118";
        }))
} :
let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      niobiumcoconut  = niobiumcoconut' { sources = sources; compiler = compiler; };
      actiniumbravohoneydew  = actiniumbravohoneydew' { sources = sources; compiler = compiler; };
    };
  };
in
myHaskellPackages.callCabal2nix "HaskellNixCabalStarter" (./.) {}
