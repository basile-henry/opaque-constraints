{ nixpkgs ? import ./nixpkgs.nix, compiler ? "ghc865" }:

nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix
  "opaque-constraints" ./.  {}
