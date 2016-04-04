{ mkDerivation, aeson, base, servant, stdenv, time }:
mkDerivation {
  pname = "money-api";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base servant time ];
  license = stdenv.lib.licenses.unfree;
}
