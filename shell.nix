{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bifunctors, bytestring
      , bytestring-to-vector, cabal-install, containers, filepath, gloss
      , gloss-juicy, hindent, hlint, lens, linear, pretty-simple, stdenv
      , text, time, vector, chronos, torsor
      }:
      mkDerivation {
        pname = "serpens";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bifunctors bytestring bytestring-to-vector containers filepath
          gloss gloss-juicy lens linear pretty-simple text time vector chronos
          torsor
        ];
        executableToolDepends = [ cabal-install hindent hlint ];
        description = "A little game featuring a serpens navigating obstacles";
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackagesAug = haskellPackages.override {
    overrides = self: super: {
      gloss-rendering = super.gloss-rendering.overrideAttrs (oldAttrs: oldAttrs // {
        patches = [ ./gloss-image-blend.patch ];
      });
    };
  };
  
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackagesAug.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
