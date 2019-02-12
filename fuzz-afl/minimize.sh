#!/bin/bash
kind=$1

test_bin="./target/release/fuzzer_script_$1"
out_dir="./out/$1"
cmin_dir="./cmin/$1"
tmin_dir="./tmin/$1"

# Check this is actually a target
stat "$test_bin">&/dev/null || {
	echo '"'$1'"' is not a fuzz target.
	exit 1;
}

# And we have an output folder
stat "$out_dir">&/dev/null || {
	echo "Output folder for fuzz target does not exist, expected $out_dir";
	exit 1;
}

cargo +nightly afl cmin -i "$out_dir" -o "$cmin_dir" -- "$test_bin" || {
	echo "Error executing corpus minimization";
	exit 1;
}

mkdir -p "$tmin_dir"
for file in $(ls -Sr "$cmin_dir");
do
	cargo +nightly afl tmin -i "$cmin_dir/$file" -o "$tmin_dir/$file" -- "$test_bin"
done

