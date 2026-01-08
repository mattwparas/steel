{
  lib,
  rustPlatform,
  curl,
  pkg-config,
  makeBinaryWrapper,
  libgit2,
  oniguruma,
  openssl,
  sqlite,
  zlib,

  includeInterpreter ? true,
  includeLSP ? true,
  includeForge ? true,
}:
let
  manifest = lib.importTOML ../Cargo.toml;
in
assert lib.assertMsg (
  includeInterpreter || includeLSP || includeForge
) "At least one of includeInterpreter, includeLSP, or includeForge must be enabled";
rustPlatform.buildRustPackage {
  pname = "steel";
  inherit (manifest.workspace.package) version;

  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.gitTracked ../.;
  };

  cargoLock.lockFile = ../Cargo.lock;

  nativeBuildInputs = [
    pkg-config
    rustPlatform.bindgenHook
    makeBinaryWrapper
  ]
  ++ lib.optionals includeForge [ curl ];

  buildInputs = [
    zlib
  ]
  ++ lib.optionals includeForge [
    curl
    libgit2
    openssl
  ]
  ++ lib.optionals includeInterpreter [
    oniguruma
    sqlite
  ];

  postPatch = ''
    rm .cargo/config.toml
  '';

  cargoBuildFlags = [
    "--package"
    "cargo-steel-lib"
  ]
  ++ lib.optionals includeInterpreter [
    "--package"
    "steel-interpreter"
  ]
  ++ lib.optionals includeLSP [
    "--package"
    "steel-language-server"
  ]
  ++ lib.optionals includeForge [
    "--package"
    "steel-forge"
  ];

  doCheck = false;

  postInstall = ''
    mkdir -p $out/lib/steel

    substituteInPlace crates/forge/installer/download.scm \
      --replace-fail '"cargo-steel-lib"' '"$out/bin/cargo-steel-lib"'

    pushd cogs
    $out/bin/steel install.scm
    popd

    mv $out/lib/steel/bin/repl-connect $out/bin
    rm -rf $out/lib/steel/bin
  '';

  postFixup =
    lib.optionalString includeInterpreter ''
      wrapProgram $out/bin/steel --set-default STEEL_HOME "$out/lib/steel"
    ''
    + lib.optionalString includeForge ''
      wrapProgram $out/bin/forge --set-default STEEL_HOME "$out/lib/steel"
    ''
    + lib.optionalString includeLSP ''
      wrapProgram $out/bin/steel-language-server --set-default STEEL_HOME "$out/lib/steel"
    ''
    + ''
      wrapProgram $out/bin/cargo-steel-lib --set-default STEEL_HOME "$out/lib/steel"
    '';

  env =
    lib.optionalAttrs includeForge { OPENSSL_NO_VENDOR = true; }
    // lib.optionalAttrs includeInterpreter { RUSTONIG_SYSTEM_LIBONIG = true; }
    // {
      STEEL_HOME = "${placeholder "out"}/lib/steel";
    };

  meta = {
    description =
      if includeInterpreter then
        "Embedded scheme interpreter in Rust"
      else if includeLSP then
        "Steel language server"
      else
        "Package manager for Steel";
    homepage = "https://github.com/mattwparas/steel";
    license = with lib.licenses; [
      asl20
      mit
    ];
    maintainers = with lib.maintainers; [ HeitorAugustoLN ];
    mainProgram =
      if includeInterpreter then
        "steel"
      else if includeLSP then
        "steel-language-server"
      else
        "forge";
    platforms = lib.platforms.unix;
    sourceProvenance = [ lib.sourceTypes.fromSource ];
  };
}
