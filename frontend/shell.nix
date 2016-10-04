{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, either, ghcjs-base, ghcjs-dom
      , ghcjs-prim, ghcjs-servant-client, money-api, reflex, reflex-dom
      , servant, stdenv, text, time, transformers, blaze-ghcjs
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
          transformers blaze-ghcjs
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
        rev = "7b4ba155200c10f800117c730c4b65ba27e59e41";
        sha256 = "0idrb2k2dkglnph5p1gh1hbqfylzjphk5d0b5r0k81vvzkpfwi2f";
      }) {};
    blaze-ghcjs = haskellPackages.callPackage /home/karshan/gits/blaze-ghcjs /*(nixpkgs.fetchgit
      {
        url = git://github.com/karshan/blaze-ghcjs;
        rev = "8dd0c8d80ae93b7277e974f2f3ba31b394fb82b4";
        sha256 = "0kn97q99l6n7pvilyacb4gnb2f6x3jyiv7bvq9vmwvl08rlav72x";
      }) {}; */
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
