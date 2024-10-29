{
  lib,
  stdenv,
  mkShell,
  steel,
  cargo,
  rustc,
  openssl,
  libiconv,
  CoreServices,
  SystemConfiguration,
  pkg-config,
  rust-analyzer,
  rustfmt,
}:
mkShell {
  shellHook = ''
    export STEEL_HOME="${steel}/lib/"
  '';
  buildInputs =
    [cargo rustc openssl libiconv]
    ++ lib.optionals stdenv.isDarwin [
      CoreServices
      SystemConfiguration
    ];
  nativeBuildInputs = [
    pkg-config
    rust-analyzer
    rustfmt
  ];
}
