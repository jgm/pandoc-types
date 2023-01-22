{
   inputs = {
     nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
     flake-utils.url = "github:numtide/flake-utils";
   };
  outputs = {self, nixpkgs, flake-utils}:
     flake-utils.lib.eachDefaultSystem (system:
       let
         builder = extraHaskellPackages:
           import ./default.nix { inherit extraHaskellPackages;
                                  pkgs = nixpkgs.legacyPackages.${system};
                                };
       in
         {
           packages.pandoc-types = builder (env: []);
           defaultPackage = self.packages.${system}.pandoc-types;
           devShell = builder (hsPkgs: with hsPkgs; [hlint ghcid]);
         });
}
