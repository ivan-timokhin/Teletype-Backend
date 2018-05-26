{ mkDerivation, base, stdenv, tdjson }:
mkDerivation {
  pname = "hstdlib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ tdjson ];
  description = "Haskell bindings for Telegram's tdlib library";
  license = stdenv.lib.licenses.bsd3;
}
