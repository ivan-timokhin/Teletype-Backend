{ mkDerivation, aeson, base, bytestring, lens, stdenv, tdjson
, unordered-containers
}:
mkDerivation {
  pname = "hstdlib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring lens unordered-containers
  ];
  librarySystemDepends = [ tdjson ];
  description = "Haskell bindings for Telegram's tdlib library";
  license = stdenv.lib.licenses.bsd3;
}
