{ mkDerivation, base, bifunctors, bytestring, bytestring-to-vector
, cabal-install, containers, filepath, gloss, gloss-juicy, hindent
, hlint, lens, linear, pretty-simple, stdenv, text, time, vector, chronos, torsor
}:
mkDerivation {
  pname = "serpens";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bifunctors bytestring bytestring-to-vector containers filepath
    gloss gloss-juicy lens linear pretty-simple text time vector chronos torsor
  ];
  executableToolDepends = [ cabal-install hindent hlint ];
  description = "A little game featuring a serpens navigating obstacles";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
