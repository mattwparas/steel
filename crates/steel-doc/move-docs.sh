# Build the docs, clear the target location, move the new docs there
cargo run && \
  rm -rf ../../docs/src/builtins && \
  mv generated/ ../../docs/src/builtins
