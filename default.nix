{ mkDerivation, base, bytestring, containers, ghci-pretty, hevm
, lens, mtl, parsers, pretty, stdenv, symbex, tasty, text, trifecta
, uniplate, unordered-containers
}:
mkDerivation {
  pname = "dslang";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers ghci-pretty hevm lens mtl parsers pretty
    symbex text trifecta uniplate unordered-containers
  ];
  executableHaskellDepends = [ base bytestring containers text ];
  testHaskellDepends = [ base bytestring tasty text ];
  homepage = "https://github.com/dapphub/dslang";
  description = "A dapp definition language";
  license = stdenv.lib.licenses.agpl3;
}
