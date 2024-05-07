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
        rustPlatform.buildRustPackage {
          pname = manifest.package.name;
          version = manifest.workspace.package.version;
          src = gitignoreSource ./.;
          cargoLock = {
            lockFile = ./Cargo.lock;
            # Temporary fix until https://github.com/mattwparas/steel/issues/192 is fixed upstream
            outputHashes = {
              "lasso-0.7.2" = "sha256-ibpHfge3nEtwLNghKEQT7ZpTe5kgDf8hbBb9qYHyHcQ=";
            };
          };
          cargoBuildFlags = "-p cargo-steel-lib -p steel-interpreter";
          buildInputs = [openssl] ++ lib.optionals stdenv.isDarwin [darwin.apple_sdk.frameworks.Security];
          nativeBuildInputs = [
            pkg-config
          ];
          # Test failing
          doCheck = false;
          postInstall = ''
            substituteInPlace cogs/installer/download.scm --replace-warn "cargo-steel-lib" "$out/bin/cargo-steel-lib"
            mkdir $out/lib
            export STEEL_HOME="$out/lib"
            pushd cogs
            $out/bin/steel install.scm
            popd
            rm "$out/bin/cargo-steel-lib"
          '';
        };
    in rec {
      formatter = pkgs.alejandra;
      packages.steel = steel;
      legacyPackages = packages;
      defaultPackage = packages.steel;
      devShell = with pkgs;
        mkShell {
          shellHook = ''
            export STEEL_HOME="${steel}/lib/"
          '';
          buildInputs = [cargo openssl libiconv] ++ lib.optionals stdenv.isDarwin [
            darwin.apple_sdk.frameworks.CoreServices
            darwin.apple_sdk.frameworks.SystemConfiguration
          ];
          nativeBuildInputs = [
            pkg-config
            rust-analyzer
            rustfmt
          ];
        };
      apps.steel = {
        type = "app";
        program = "${steel}/bin/steel";
      };
      apps.default = apps.steel;
    });
}
