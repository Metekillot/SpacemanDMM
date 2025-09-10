//! Example command-line tool to unpack and display a proc's block
//!
//! Usage: cargo run --example unpack_block -- <path/to/file.dme> <proc/path>
//! Example: cargo run --example unpack_block -- game.dme /mob/proc/Move

use std::env;
use std::process;
use dreammaker::{Context, objtree::ObjectTree, parser::Parser, preprocessor::Preprocessor, indents::IndentProcessor};
use std::path::{PathBuf};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 4 {
        eprintln!("Usage: {} <dme_file> <type_path> <proc_name>", args[0]);
        eprintln!("Example: {} MATERIAL_TO_TEST/example.dme /datum /demonstration_proc", args[0]);
        process::exit(1);
    }

    let dme_path = PathBuf::from(&args[1]);
    let type_path = &args[2];
    let proc_name = &args[3];

    // Parse the DME file
    let mut context = Context::default();
    let mut parser = Parser::new(&context, IndentProcessor::new(&context, Preprocessor::new(&context, dme_path.to_owned()).unwrap()));
    parser.enable_procs();
    let objtree = parser.parse_object_tree();

    let type_result = objtree.find(type_path).expect("Type not found.");
    let proc_ref_found = type_result.get_proc(proc_name).expect("Proc not found.");
    let proc_name = proc_ref_found.name();
    let proc_value = proc_ref_found.get();
    let proc_code_block = proc_value.code.as_ref().expect("Proc has no code block.");

    // Extract and validate the proc path

    println!("// Proc: {}", proc_name);
    println!("// Location: {:?}", proc_value.location);
    println!();
    let unpacked = dreammaker::block_unpacker::unpack_block(proc_code_block);
    print!("{}", unpacked);

}
