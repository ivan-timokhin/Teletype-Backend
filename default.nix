{ nixpkgs ? import ./nixpkgs.nix } :

let
  compiler = "ghc822";
in with nixpkgs; rec {
  tdlib = stdenv.mkDerivation {
    name = "tdlib-1.2.0";
    src = fetchFromGitHub {
      owner = "tdlib";
      repo = "td";
      rev = "cfe4d9bdcee9305632eb228a46a95407d05b5c7a";
      sha256 = "0445hiqp2gmkd60kcv7r10li7k09bjrzy3ywd43iwc99jay1pwc1";
    };

    propagatedBuildInputs = [openssl zlib];
    nativeBuildInputs = [cmake gperf];

    meta = {
      description = "Cross-platform library for building Telegram clients";
      longDescription = ''
        TDLib (Telegram Database library) is a cross-platform library for building Telegram clients.
        It can be easily used from almost any programming language.
      '';
      homepage = https://core.telegram.org/tdlib;
      license = stdenv.lib.licenses.boost;
    };
  };

  hstdlib = import ./hstdlib { pkgs = nixpkgs; inherit tdlib; inherit compiler; };

  teletype-server = import ./teletype-server { pkgs = nixpkgs; inherit hstdlib; inherit compiler; };
}
