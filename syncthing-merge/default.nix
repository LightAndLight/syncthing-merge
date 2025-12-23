{ mkDerivation, aeson, base, bytestring, containers, deepseq
, diagnostica, diagnostica-sage, directory, filepath, http-client
, http-types, lib, mtl, optparse-applicative, process, sage, text
, time
}:
mkDerivation {
  pname = "syncthing-merge";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers deepseq diagnostica
    diagnostica-sage directory filepath http-client http-types mtl
    optparse-applicative process sage text time
  ];
  license = lib.licenses.gpl3Only;
  mainProgram = "syncthing-merge";
}
