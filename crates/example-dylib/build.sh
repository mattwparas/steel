cargo build --release
cd ../../
cp target/release/libexample_dylib.so $STEEL_HOME/native/
cp target/release/libexample_dylib.dylib $STEEL_HOME/native/
cp target/release/libexample_dylib.d $STEEL_HOME/native/