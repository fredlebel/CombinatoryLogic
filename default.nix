#with import <nixpkgs> {};
#stdenv.mkDerivation rec {
#  name = "env";
#  env = buildEnv { name = name; paths = buildInputs; };
#  buildInputs = [
#    ghc
#    cabal-install
#  ];
#}

{ pkgs ? import <nixpkgs> {} } :
with pkgs; pkgs.mkShell {

  packages = [
    cabal-install
    haskellPackages.haskell-language-server
  ];

  C_INCLUDE_PATH =  "${ncurses.dev}/include";
  LIBRARY_PATH =    "${ncurses}/lib";
  LD_LIBRARY_PATH = "${zlib}/lib";

}
