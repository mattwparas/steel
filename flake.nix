{
  description = "Embedded scheme interpreter in Rust";

  inputs = {
    flake-compat.url = "github:edolstra/flake-compat";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem =
        { pkgs, self', ... }:
        {
          devShells.default = pkgs.callPackage ./nix/shell.nix { inherit (self'.packages) steel; };

          packages = {
            default = self'.packages.steel;
            steel = pkgs.callPackage ./nix/package.nix { };
          };
        };
    };
}
