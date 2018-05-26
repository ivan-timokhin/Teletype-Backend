{ mkDerivation, aeson, base, bytestring, stdenv, tdjson }:
mkDerivation {
  pname = "hstdlib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bytestring ];
  librarySystemDepends = [ tdjson ];
  description = "Haskell bindings for Telegram's tdlib library";
  license = stdenv.lib.licenses.bsd3;
}
