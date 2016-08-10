{ mkDerivation, aeson, base, servant, stdenv, time, parsec, lens }:
mkDerivation {
  pname = "money-api";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base servant time parsec lens ];
  license = stdenv.lib.licenses.unfree;
}
