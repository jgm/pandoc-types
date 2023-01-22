{ pkgs ? import <nixpkgs> {}
, haskellVersion ? null
}:
let
  extraHaskellPackages = hsPkgs: with hsPkgs; [hlint ghcid];
in import ./default.nix { inherit pkgs haskellVersion extraHaskellPackages; }
