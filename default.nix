{ pkgs ? import <nixpkgs> {}
, haskellVersion ? null
, extraHaskellPackages ? (env: [])
}:
let
  hsPkgs = if isNull haskellVersion
           then pkgs.haskellPackages
           else pkgs.haskell.packages."ghc${haskellVersion}";
in hsPkgs.developPackage {
  name = "pandoc-types";
  root = pkgs.nix-gitignore.gitignoreSourcePure [
    "dist-newstyle"
    ".git"
    ".*#"
    "flake.lock"
  ] ./.;
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools =
      let extraPackages = extraHaskellPackages hsPkgs;
      in (attrs.buildTools or []) ++ extraPackages;
  });
}
