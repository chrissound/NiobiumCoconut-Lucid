{ sources ? import ./nix/sources.nix
, compiler ? "ghc865"
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
      niobiumcoconut  = self.callCabal2nix "niobiumcoconut"
        (/home/chris/fromLaptopt/usbflash/Haskell/NiobiumCoconut/.) {};
      actiniumbravohoneydew  = self.callCabal2nix "actiniumbravohoneydew"
        (/home/chris/NewProjects/ActiniumBravoHoneydew/.) {};
    };
  };
in
myHaskellPackages.callCabal2nix "HaskellNixCabalStarter" (./.) {}
