{ nixpkgs ? import ./pinned.nix {}, compiler ? "ghc883", withHoogle ? true }:

let
  inherit (nixpkgs) pkgs;
  forecast = pkgs.callPackage ./. {};
in
forecast.overrideAttrs (drv: {
  buildInputs = drv.buildInputs ++ [ pkgs.cabal-install ];
})
