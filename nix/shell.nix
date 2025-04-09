{ lib
, stdenv
, mkShell
, steel
, cargo
, rustc
, libiconv
, CoreServices
, SystemConfiguration
, rust-analyzer
, rustfmt
,
}:
mkShell {
  shellHook = ''
    export STEEL_HOME="${steel}/lib/"
  '';
  packages =
    [
      cargo
      rustc
      rust-analyzer
      rustfmt
      libiconv
    ]
    ++ lib.optionals stdenv.isDarwin [
      CoreServices
      SystemConfiguration
    ];
  inputsFrom = [ steel ];
}
