{ mkDerivation, async, base, configurator, hstdlib
, optparse-applicative, servant, servant-server, stdenv, text
, transformers, vector, warp
}:
mkDerivation {
  pname = "teletype-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base configurator hstdlib optparse-applicative servant
    servant-server text transformers vector warp
  ];
  license = stdenv.lib.licenses.mit;
}
