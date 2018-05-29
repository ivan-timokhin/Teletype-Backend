{ mkDerivation, aeson, base, bytestring, lens, stdenv, tdjson, text
, unordered-containers
}:
mkDerivation {
  pname = "hstdlib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring lens text unordered-containers
  ];
  librarySystemDepends = [ tdjson ];
  description = "Haskell bindings for Telegram's tdlib library";
  license = stdenv.lib.licenses.bsd3;
}
