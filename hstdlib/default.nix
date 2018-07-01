{ pkgs ? import <nixpkgs-unstable> {}, tdlib }:
pkgs.haskell.packages.ghc843.callPackage ./package.nix { tdjson = tdlib; }
