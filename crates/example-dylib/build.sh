cargo build --release
cd ../
cp target/release/libexample_dylib.so $HOME/.steel/native/
cp target/release/libexample_dylib.dylib $HOME/.steel/native/
cp target/release/libexample_dylib.d $HOME/.steel/native/