{ mkDerivation, base, bytestring, directory, gargoyle, posix-escape, process
, stdenv, stringsearch, text, unix, postgresql
}:
mkDerivation {
  pname = "gargoyle-postgresql";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory gargoyle posix-escape process stringsearch text unix
  ];
  executableHaskellDepends = [
    base bytestring gargoyle process text unix
  ];
  testHaskellDepends = [
    postgresql
  ];
  description = "Manage PostgreSQL servers with gargoyle";
  license = stdenv.lib.licenses.bsd3;
}
