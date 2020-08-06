{ nixpkgs ? import ./pinned.nix {}, compiler ? "ghc883" }:

let
  hpkgs = nixpkgs.haskell.packages;
  forecast = hpkgs.${compiler}.callCabal2nix "forecast" ./. {};

in forecast.env.overrideAttrs (drv: {
  buildInputs = drv.buildInputs ++ [ ];
})
