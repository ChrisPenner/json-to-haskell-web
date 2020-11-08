{ mkDerivation, aeson, aeson-extra, base, bimap, bytestring, casing
, containers, hspec, microlens-platform, mtl, nonempty-containers
, raw-strings-qq, recursion-schemes, stdenv, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "json-to-haskell";
  version = "0.0.1.0";
  sha256 = "447030f4cd91d65b932bf77aefc0cc176932ec322ca59685fdc3ab9f5260aa8c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-extra base bimap casing containers microlens-platform
    mtl nonempty-containers recursion-schemes text unordered-containers
    vector
  ];
  executableHaskellDepends = [
    aeson aeson-extra base bimap bytestring casing containers
    microlens-platform mtl nonempty-containers raw-strings-qq
    recursion-schemes text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson aeson-extra base bimap casing containers hspec
    microlens-platform mtl nonempty-containers recursion-schemes text
    unordered-containers vector
  ];
  homepage = "https://github.com/githubuser/json-to-haskell#readme";
  license = stdenv.lib.licenses.bsd3;
}
