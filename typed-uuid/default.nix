{ mkDerivation, aeson, autodocodec, base, binary, bytestring
, deepseq, hashable, http-api-data, lib, random, text, uuid
, validity, validity-uuid
}:
mkDerivation {
  pname = "typed-uuid";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base binary bytestring deepseq hashable
    http-api-data random text uuid validity validity-uuid
  ];
  homepage = "https://github.com/NorfairKing/typed-uuid#readme";
  description = "Phantom-Typed version of UUID";
  license = lib.licenses.mit;
}
