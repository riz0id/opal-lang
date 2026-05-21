{ mkDerivation, base, fetchgit, lib, regex-posix, template-haskell
}:
mkDerivation {
  pname = "language-haskell-extract";
  version = "0.2.3";
  src = fetchgit {
    url = "https://github.com/riz0id/template-helper";
    sha256 = "0gv4b2qd0ld9b8fm7i3xx5rq4i1pvs4vxmad6njzr319y64ndz7z";
    rev = "3aa67d5cb316c12cd8f22384cbb9d5999a49598e";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base regex-posix template-haskell ];
  homepage = "http://github.com/finnsson/template-helper";
  description = "Module to automatically extract functions from the local code";
  license = lib.licenses.bsd3;
}
