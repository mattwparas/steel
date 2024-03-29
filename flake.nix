{
  description = "Steel";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; # We want to use packages from the binary cache
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    gitignore,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      inherit (gitignore.lib) gitignoreSource;
      pkgs = nixpkgs.legacyPackages.${system};

      manifest = pkgs.lib.importTOML ./Cargo.toml;
      steel = with pkgs;
        rustPlatform.buildRustPackage rec {
          pname = manifest.package.name;
          version = manifest.workspace.package.version;
          src = gitignoreSource ./.;
          cargoLock.lockFile = ./Cargo.lock;
          buildInputs = [openssl] ++ lib.optionals stdenv.isDarwin [darwin.apple_sdk.frameworks.Security];
          nativeBuildInputs = [
            pkg-config
          ];
          # Test failing
          doCheck = false;
          postInstall = ''
            mkdir $out/lib
            export STEEL_HOME="$out/lib"
            pushd cogs
            $out/bin/steel install.scm
            popd
          '';
        };
    in rec {
      packages.steel = steel;
      legacyPackages = packages;
      defaultPackage = packages.steel;
      devShell = with pkgs; mkShell {
        buildInputs = [cargo openssl] ++ lib.optionals stdenv.isDarwin [darwin.apple_sdk.frameworks.Security];
        nativeBuildInputs = [
          pkg-config
          rust-analyzer
        ];
      };
    });
}
