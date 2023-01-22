{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      # let
      #   mkPkg = env:
      #     nixpkgs.haskellPackages.developPackage {
      #       name = "pandoc-types";
      #       root = nixpkgs.lib.sourceFilesBySufficies ./.
      #         [ ".cabal"
      #           ".project"
      #           ".hs"
      #           "LICENSE"
      #           "stack.yaml"
      #         ];
      #     };
      # in {
      let
        builder = extraHaskellPackages:
          import ./default.nix { inherit extraHaskellPackages; pkgs = nixpkgs; };
      in {
        packages.pkg = builder (env: []);
        defaultPackage = self.packages.${system}.pkg;
        devShell = builder (hsPkgs: with hsPkgs; [hlint ghcid]);
      });
}
