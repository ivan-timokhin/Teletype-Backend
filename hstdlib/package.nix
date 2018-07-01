{ mkDerivation, aeson, async, base, bytestring, containers
, generic-lens, lens, stdenv, stm, tdjson, text, time
, unordered-containers, vector
}:
mkDerivation {
  pname = "hstdlib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers generic-lens lens stm text
    time unordered-containers vector
  ];
  librarySystemDepends = [ tdjson ];
  description = "Haskell bindings for Telegram's tdlib library";
  license = stdenv.lib.licenses.mit;
}
