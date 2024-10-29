{
  lib,
  stdenv,
  Security,
  openssl,
  pkg-config,
  rustPlatform,
}: let
  manifest = lib.importTOML ../Cargo.toml;
  fs = lib.fileset;
in
  rustPlatform.buildRustPackage {
    pname = manifest.package.name;
    version = manifest.workspace.package.version;
    src = fs.toSource {
      root = ../.;
      fileset =
        fs.intersection
        (fs.gitTracked ../.)
        (fs.fileFilter
          (f:
            f.name
            == "Cargo.lock"
            || f.hasExt "rkt"
            || f.hasExt "rs"
            || f.hasExt "scm"
            || f.hasExt "toml")
          ../.);
    };
    cargoLock = {
      lockFile = ../Cargo.lock;
    };
    cargoBuildFlags = "-p cargo-steel-lib -p steel-interpreter";
    buildInputs = [openssl] ++ lib.optionals stdenv.isDarwin [Security];
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
    meta = {
      description = "An embedded scheme interpreter in Rust";
      homepage = "https://github.com/mattwparas/steel";
      license = with lib.licenses; [asl20 mit];
      mainProgram = "steel";
    };
  }
