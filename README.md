# `remolt` - Reduced Embeddable More Or Less Tcl

This is a fork of [Molt], a Rust implementation of a Tcl-like language. The goal
of this fork is to produce a Tcl-like system that can run on microcontrollers
without an operating system. In pursuit of that goal, the main changes are:

1. `remolt` is compatible with `no_std` platforms as long as the `alloc` crate
   is available.

2. Chunks of the implementation have been rewritten with a focus on reducing
   the size required in ROM (or, in Unix-speak, the "text size").

3. Many parts of the implementation have been made optional, controlled through
   Cargo features, allowing the build to be tailored for your specific
   requirements.

4. To improve performance on microcontrollers, several algorithms and code
   patterns have been rewritten to reduce intermediate allocations and copies.

In addition, in revisiting the Molt codebase from four years later, some
opportunities for modernization and simplification became available:

- The system now uses the Rust 2021 edition.

- Patterns in the code that replicate common API (like `unwrap_or_default` or
  `TryFrom`) have been replaced with calls to the standard API.

- The "context map" facility has been removed in favor of the ability to
  register arbitrary Rust closures as Tcl commands.

- The codebase is now warning-free up through rustc 1.77.2, including Clippy.

The result is unfortunately not perfectly compatible with Molt -- in particular,
the interface for providing custom commands is different. However, it's a lot
smaller. Specifically:

- With the minimum set of changes to Molt to compile for
  `thumbv7em-none-eabihf`, a small driver program that blinks an LED from Tcl
  requires **198,184** bytes of ROM.

- Using `remolt` with all features except `eval` turned off, the same program
  requires **60,912** bytes of ROM --- smaller by 70%.

- Further turning off `eval` (to disable all expression parsing, making the
  engine behave roughly like partcl) brings it down to **49,048** bytes of ROM,
  but in practice you probably want expression parsing enabled.

## Acknowledgements

Most of the code in this repo is from the Molt contributors. I only optimized
the heck out of it.

[Molt]: https://github.com/wduquette/molt
