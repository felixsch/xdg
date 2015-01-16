{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall transformers parsec
                            process filepath directory;
in cabal.mkDerivation (self: {
  pname = "drivingthesky";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    transformers
    parsec
    process
    filepath
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
