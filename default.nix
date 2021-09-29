{ mkDerivation, base, bytestring, case-insensitive, containers
, data-default, exceptions, ghcjs-dom, http-api-data, http-media
, http-types, jsaddle, lib, mtl, network-uri, reflex
, reflex-dom-core, reflex-dom, safe, servant, servant-auth, sop-core
, string-conversions, text, transformers
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.6";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    ghcjs-dom http-api-data http-media http-types jsaddle mtl
    network-uri reflex reflex-dom-core reflex-dom safe servant servant-auth
    sop-core string-conversions text transformers
  ];
  description = "servant API generator for reflex apps";
  license = lib.licenses.bsd3;
}
