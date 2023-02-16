cargo build --release
cd ../../
cp target/release/libsteel_webserver.so $STEEL_HOME/native/
cp target/release/libsteel_webserver.dylib $STEEL_HOME/native/
cp target/release/libsteel_webserver.d $STEEL_HOME/native/