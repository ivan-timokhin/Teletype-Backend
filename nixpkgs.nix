let
  hostPkgs = import <nixpkgs> {};
  pinnedPkgs = hostPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "fda46a645c962a7ae01680a0a0e290a6f0b5de1b";
    sha256 = "0zf76q5pcxz7qn9hqh3x783qvjiip6zsq2iph55q4wg9kdyhx3zz";
  };
in import pinnedPkgs {}
