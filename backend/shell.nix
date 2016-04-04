{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, aeson, base, base64-bytestring
      , bytestring, case-insensitive, containers, cryptonite
      , data-default-class, either, HandsomeSoup, http-client, http-types
      , hxt, lens, lens-aeson, memory, money-api, mtl, network
      , pretty-show, safecopy, servant-server, split, stdenv, text, time
      , transformers, unordered-containers, utf8-string, wai, warp, wreq
      }:
      mkDerivation {
        pname = "money";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          acid-state aeson base base64-bytestring bytestring case-insensitive
          containers cryptonite data-default-class either HandsomeSoup
          http-client http-types hxt lens lens-aeson memory money-api mtl
          network pretty-show safecopy servant-server split text time
          transformers unordered-containers utf8-string wai warp wreq
        ];
        buildTools = [ pkgs.cabal-install ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f { money-api = haskellPackages.callPackage ../api {}; };

in

  if pkgs.lib.inNixShell then drv.env else drv
