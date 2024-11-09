{
  description = "Steel";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = {
    self,
    nixpkgs,
    systems,
  }: let
    eachSystem = nixpkgs.lib.genAttrs (import systems);
    pkgsFor = nixpkgs.legacyPackages;
  in {
    packages = eachSystem (system: {
      default = self.packages.${system}.steel;
      steel = pkgsFor.${system}.callPackage ./nix/package.nix {
        inherit (pkgsFor.${system}.darwin.apple_sdk.frameworks) Security;
      };
    });

    formatter = eachSystem (system: pkgsFor.${system}.alejandra);

    # DEPRECATED
    legacyPackages = self.packages;
    defaultPackage = eachSystem (system: self.packages.${system}.default);

    devShells = eachSystem (system: {
      default = pkgsFor.${system}.callPackage ./nix/shell.nix {
        inherit (self.packages.${system}) steel;
        inherit (pkgsFor.${system}.darwin.apple_sdk.frameworks) CoreServices SystemConfiguration;
      };
    });

    apps = eachSystem (system: {
      steel = {
        type = "app";
        program =
          pkgsFor.${system}.lib.getExe
          self.packages.${system}.steel;
      };
      default = self.apps.${system}.steel;
    });
  };
}
