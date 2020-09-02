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
        (builtins.fetchGit {
          url = "https://github.com/chrissound/NiobiumCoconut.git";
          rev = "18886b6276e61d62b9bfb5cfb71b8892c9fd2d30";
        }) {};
      actiniumbravohoneydew  = self.callCabal2nix "actiniumbravohoneydew"
        (builtins.fetchGit {
          url = "https://github.com/chrissound/ActiniumBravoHoneydew.git";
          rev = "da77b13b9a9b9b5ba1c34a3813e252d72535bb54";
        }) {};
    };
  };
in
myHaskellPackages.callCabal2nix "HaskellNixCabalStarter" (./.) {}
