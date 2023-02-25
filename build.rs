fn main() {
    let llvm_dir = env!("LLVM_DIR", "Please provide LLVM_DIR");
    println!("cargo:rustc-link-search=native={}", llvm_dir);
    println!("cargo:rustc-link-lib=static=LLVMCore");
    println!("cargo:rustc-link-lib=static=LLVMBitWriter");
    println!("cargo:rustc-link-lib=static=LLVMIRReader");
}
