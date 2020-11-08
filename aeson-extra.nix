{ mkDerivation, aeson, aeson-compat, attoparsec, attoparsec-iso8601
, base, base-compat-batteries, bytestring, containers, deepseq
, exceptions, hashable, parsec, quickcheck-instances
, recursion-schemes, scientific, semialign, stdenv, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text, these
, time, time-parsers, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson-extra";
  version = "0.4.1.3";
  sha256 = "22e3e2b6d243fb9bc660899dbb677cb6f1f8c348cfc7464082b60ce3fcdc25cc";
  revision = "4";
  editedCabalFile = "0gwjgxpgq7lncylfpccikmn3jk2jmz54vsgjialhwa26iv9f9n4a";
  libraryHaskellDepends = [
    aeson aeson-compat attoparsec attoparsec-iso8601 base
    base-compat-batteries bytestring containers deepseq exceptions
    hashable parsec recursion-schemes scientific semialign
    template-haskell text these time unordered-containers vector
  ];
  testHaskellDepends = [
    base containers quickcheck-instances tasty tasty-hunit
    tasty-quickcheck time time-parsers unordered-containers vector
  ];
  homepage = "https://github.com/phadej/aeson-extra#readme";
  description = "Extra goodies for aeson";
  license = stdenv.lib.licenses.bsd3;
}
