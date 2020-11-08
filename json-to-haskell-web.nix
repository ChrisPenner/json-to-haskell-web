{ mkDerivation, aeson, base, json-to-haskell, miso, stdenv }:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ aeson base json-to-haskell miso ];
  description = "First miso app";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
