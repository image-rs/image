pub fn read_file_from_args() -> Vec<u8> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <path-to-crash>", args[0]);
        std::process::exit(1);
    }
    let data = std::fs::read(&args[1])
        .expect(&format!("Could not open file {}", args[1]));
    data
}
