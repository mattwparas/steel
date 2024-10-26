{
  description = "Steel";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    eachSystem = nixpkgs.lib.genAttrs systems;
    pkgsFor = nixpkgs.legacyPackages;
  in {
    packages = eachSystem (system: {
      default = self.packages.${system}.steel;
      steel = pkgsFor.${system}.callPackage ./nix/package.nix {};
    });

    formatter = eachSystem (system: pkgsFor.${system}.alejandra);
    legacyPackages = self.packages;
    defaultPackage = eachSystem (system: self.packages.${system}.default);

    devShells = eachSystem (system: {
      default = pkgsFor.${system}.callPackage ./nix/package.nix {};
    });

    apps = eachSystem (system: {
      steel = {
        type = "app";
        program = "${self.packages.${system}.steel}/bin/steel";
      };
      default = self.apps.${system}.steel;
    });
  };
}
