{
  mkShell,
  cargo,
  clippy,
  rust-analyzer,
  rustc,
  rustfmt,
  steel,
}:
mkShell {
  inputsFrom = steel;

  packages =
    [
      cargo
      clippy
      rustc
      rust-analyzer
      rustfmt
      steel
    ];

  strictDeps = true;
}
