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

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.treefmt-nix.flakeModule ];

      systems = import inputs.systems;

      perSystem =
        { pkgs, self', ... }:
        {
          devShells.default = pkgs.callPackage ./nix/shell.nix { inherit (self'.packages) steel; };

          packages = {
            default = self'.packages.steel;
            steel = pkgs.callPackage ./nix/package.nix { };

            steel-forge = pkgs.callPackage ./nix/package.nix {
              includeInterpreter = false;
              includeLSP = false;
            };

            steel-interpreter = pkgs.callPackage ./nix/package.nix {
              includeForge = false;
              includeLSP = false;
            };

            steel-language-server = pkgs.callPackage ./nix/package.nix {
              includeForge = false;
              includeInterpreter = false;
            };
          };

          treefmt = {
            flakeCheck = true;

            programs = {
              nixfmt.enable = true;
              rustfmt.enable = true;
            };

            projectRootFile = "flake.nix";
          };
        };
    };
}
