{ mkDerivation, base, fetchgit, lib, regex-posix, template-haskell
}:
mkDerivation {
  pname = "language-haskell-extract";
  version = "0.2.3";
  src = fetchgit {
    url = "https://github.com/finnsson/template-helper";
    sha256 = "1wyrm7qyq2ia888fvhshqkl7fwh9b7h880fh3xgz0vv1r6d40nyd";
    rev = "9047b3793aac904052dcb3f1704b53d6d41d2374";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base regex-posix template-haskell ];
  homepage = "http://github.com/finnsson/template-helper";
  description = "Module to automatically extract functions from the local code";
  license = lib.licenses.bsd3;
}
