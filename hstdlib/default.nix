{ pkgs ? import <nixpkgs-unstable> {}, tdlib }:
pkgs.haskell.packages.ghc842.callPackage ./package.nix { tdjson = tdlib; }
