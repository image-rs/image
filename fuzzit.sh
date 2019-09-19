# Arguments: fuzz target-id, fuzzer name
fuzzer_target="$1"
fuzzer_binary="$2"

# Get cargo-fuzz
cargo install cargo-fuzz
# Build the fuzzer binary
cargo fuzz run "$fuzzer_binary" -- -runs=0

wget -O fuzzit https://github.com/fuzzitdev/fuzzit/releases/download/v2.4.55/fuzzit_Linux_x86_64
chmod a+x fuzzit
./fuzzit create job --type local-regression "$fuzzer_target" "./fuzz/target/x86_64-unknown-linux-gnu/debug/$fuzzer_binary"
