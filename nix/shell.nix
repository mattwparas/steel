{
  lib,
  stdenv,
  mkShell,
  steel,
  cargo,
  rustc,
  openssl,
  libiconv,
  darwin,
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
      darwin.apple_sdk.frameworks.CoreServices
      darwin.apple_sdk.frameworks.SystemConfiguration
    ];
  nativeBuildInputs = [
    pkg-config
    rust-analyzer
    rustfmt
  ];
}
