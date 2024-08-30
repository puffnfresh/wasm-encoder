{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, self }:
    let forEachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
    in {
      devShells = forEachSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.cabal-install
              (pkgs.haskellPackages.ghcWithPackages (p: [
                p.cereal
                p.tasty
                p.tasty-golden
              ]))
            ];
          };
        });
    };
}
