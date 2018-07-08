{ pkgs ? import <nixpkgs-unstable> {}, hstdlib, compiler }:
pkgs.haskell.packages.${compiler}.callPackage ./package.nix {
  inherit hstdlib;
}
