with import <nixpkgs> {};
let edf = pkgs.haskellPackages.callPackage ../default.nix {};
    ghc' = ghc.withPackages (ps: with ps; [text gnuplot edf]);
in
  pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = [ghc' gnuplot curl];
  }
