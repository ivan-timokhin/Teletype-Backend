{ pkgs ? import <nixpkgs-unstable> {}, tdlib }:
#(pkgs.haskellPackages.developPackage { root = ./.; tdjson = tdlib; })
#{ tdjson = tdlib; }
pkgs.haskell.packages.ghc842.callPackage ./package.nix { tdjson = tdlib; }
