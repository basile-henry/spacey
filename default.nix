{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:

with nixpkgs.pkgs.haskell;

lib.failOnAllWarnings (packages.${compiler}.callCabal2nix "server" ./.  {
})
