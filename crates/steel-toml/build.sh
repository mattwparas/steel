cargo build --release
cd ../../
cp target/release/libsteel_toml.so $STEEL_HOME/native/
cp target/release/libsteel_toml.dylib $STEEL_HOME/native/
cp target/release/libsteel_toml.d $STEEL_HOME/native/