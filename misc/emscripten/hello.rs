// emscripten uses le32-unknown-nacl triple but rustc doesn't know it now.
// So just use similar target instead.
// `rustc hello.rs --target=i686-unknown-linux --emit-llvm -S --cfg libc`
// `emcc hello.ll -o hello.js`
//rustc --emit=llvm-bc hello.rs
//rustc --emit=llvm-ir hello.rs


// no `extern mod`.

#![feature(lang_items)]
#[no_std]
#[feature(macro_rules)]

//use core::container::Container;

// https://github.com/thestinger/rust-core
//mod core;

fn main() {
    //core::io::stdout().write(bytes!("hello world"));
}

/*#[lang="start"]
fn start(_: *mut u8, _: int, _: *mut u8) -> int {
    main();
    0
}*/
