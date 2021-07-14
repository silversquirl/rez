# rez

NOTE: rez is currently a work in progress and is missing many important features.

rez is a fast and simple regex engine for Zig. It has extremely consistent speed and memory
characteristics, making it a good choice for performance-critical applications.

## Performance

Most regex engines are prone to denial of service attacks when accepting user-specified patterns,
as they have O(2^n) worst-case time complexity. rez uses an [NFA-based matching algorithm][nfa-regex],
which has O(n) worst-case time complexity, and is hence immune to these attacks.

[nfa-regex]: https://swtch.com/~rsc/regexp/regexp1.html

## comptime compilation

For improved performance, regex compilation is performed at comptime. This allows memory usage bounds
to be precomputed, meaning matching does not require heap allocation.

## Future plans

- Full regex syntax and capture groups
- Compile to DFAs instead of NFAs
- Runtime compilation
- Compile DFAs to bytecode
- Compile comptime patterns to machine code
