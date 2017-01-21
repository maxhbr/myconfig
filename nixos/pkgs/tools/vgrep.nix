{ mkDerivation, fetchgit
, aeson, async, attoparsec, base, cabal-file-th , containers, directory
, doctest, fingertree, generic-deriving , lens, lifted-base, mmorph, mtl, pipes
, pipes-concurrency, process , QuickCheck, stdenv, stm, tasty, tasty-quickcheck
, template-haskell, text, transformers, unix, vty, yaml
}:
mkDerivation {
  pname = "vgrep";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/fmthoma/vgrep";
    sha256 = "0nk5ay80w96l2idpiqnpk04yh5wrgvh3szk18vfm1f2bgfav114x";
    rev = "1c76e22eda397a261b97998330dea834f9824db0";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base containers directory fingertree
    generic-deriving lens lifted-base mmorph mtl pipes
    pipes-concurrency process stm text transformers unix vty yaml
  ];
  executableHaskellDepends = [
    async base cabal-file-th containers directory lens mtl pipes
    pipes-concurrency process template-haskell text vty
  ];
  testHaskellDepends = [
    base containers doctest lens QuickCheck tasty tasty-quickcheck text
  ];
  homepage = "http://github.com/fmthoma/vgrep#readme";
  description = "A pager for grep";
  license = stdenv.lib.licenses.bsd3;
}
