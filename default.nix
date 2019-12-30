{ mkDerivation, base, binary, bytestring, stdenv, text }:
mkDerivation {
  pname = "edf";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base binary bytestring text ];
  homepage = "https://github.com/enomsg/edf";
  description = "EDF parsing library";
  license = stdenv.lib.licenses.bsd2;
}
