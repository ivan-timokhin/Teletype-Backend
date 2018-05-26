{ nixpkgs ? import <nixpkgs-unstable> {} }:
(import ../default.nix { inherit nixpkgs; }).hstdlib.env
