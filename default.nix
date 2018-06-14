with import <nixpkgs> {};
with import(fetchTarball https://github.com/domenkozar/hie-nix/tarball/master) {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    ghc
    cabal-install
    hie80
  ];
}
