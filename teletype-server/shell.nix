{ nixpkgs ? import ../nixpkgs.nix }:
(import ../default.nix { inherit nixpkgs; }).teletype-server.env
