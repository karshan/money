{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, either, ghcjs-base, ghcjs-dom
      , ghcjs-prim, ghcjs-servant-client, money-api, reflex, reflex-dom
      , servant, stdenv, text, time, transformers
      }:
      mkDerivation {
        pname = "money-frontend";
        version = "0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base either ghcjs-base ghcjs-dom ghcjs-prim
          ghcjs-servant-client money-api reflex reflex-dom servant text time
          transformers
        ];
        buildTools = [ pkgs.cabal-install pkgs.haskellPackages.ghcid ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f { 
    money-api = haskellPackages.callPackage ../api {}; 
    ghcjs-servant-client = haskellPackages.callPackage (nixpkgs.fetchgit 
      {
        url = git://github.com/karshan/ghcjs-servant-client;
        rev = "81a5e2b60d4d5e5baeee2505e5c2b56d11fddf0a";
        sha256 = "1rd11ch0zwj0lrly6yfgxs4nh61vnnmfh7p2la3szijszc7qapyn";
      }) {};
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
