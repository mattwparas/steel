{
  description = "Steel";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; # We want to use packages from the binary cache
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      fs = pkgs.lib.fileset;

      manifest = pkgs.lib.importTOML ./Cargo.toml;
      steel = with pkgs;
        rustPlatform.buildRustPackage {
          pname = manifest.package.name;
          version = manifest.workspace.package.version;
          src = fs.toSource {
            root = ./.;
            fileset = fs.intersection
              (fs.gitTracked ./.)
              (fs.fileFilter
                (f: f.name == "Cargo.lock" ||
                    f.hasExt "rkt" ||
                    f.hasExt "rs" ||
                    f.hasExt "scm" ||
                    f.hasExt "toml")
                ./.);
          };
          cargoLock = {
            lockFile = ./Cargo.lock;
            # Temporary fix until https://github.com/mattwparas/steel/issues/192 is fixed upstream
            outputHashes = {
              "lasso-0.7.2" = "sha256-ccqcDvWZD5hp4iZ420jgNJtR+MaVlqHFtXU2GWkbyfg=";
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
          meta = with lib; {
            description = "An embedded scheme interpreter in Rust";
            homepage = "https://github.com/mattwparas/steel";
            license = with licenses; [asl20 mit];
            mainProgram = "steel";
          };
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
