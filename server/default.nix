{ nixpkgs ? import ../nixpkgs.nix }:

with nixpkgs.pkgs.haskell;

lib.failOnAllWarnings (packages.ghc844.callCabal2nix "server" ./.  {
})
