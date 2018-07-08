{ mkDerivation, base, configurator, hstdlib, servant
, servant-server, stdenv, warp
}:
mkDerivation {
  pname = "teletype-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base configurator hstdlib servant servant-server warp
  ];
  license = stdenv.lib.licenses.mit;
}
