{ pkgs ? import <nixpkgs-unstable> {}, tdlib, compiler }:
pkgs.haskell.packages.${compiler}.callPackage ./package.nix { tdjson = tdlib; }
