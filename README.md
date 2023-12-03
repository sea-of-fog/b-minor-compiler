# B-minor compiler

## Changes to the language
1. The compiler is named *bmc*
2. The extension for B- files is *.bm*
3. Declarations use let - that's easier to parse
4. Booleans are encoded as $0 or $(-1)

## Todo
1. [X] Generate code for print
2. [X] Booleans
3. [ ] Division operator in codegen
4. [ ] Boolean operators in codegen
5. [X] Add interfaces to modules
6. [X] Typechecking/type inference
7. [ ] Print booleans
8. [X] Parsing whitespace
9. [ ] Add unit tests
10. [ ] Testing infrastructure
11. [ ] Fix bug in the compiler app
12. [X] Add types to declarations
13. [ ] If/else
14. [ ] Uninitialised declarations
15. [ ] Check for initialised declarations
16. [ ] Loops
17. [ ] Arrays
18. [ ] Function primitives
19. [ ] Function declarations
20. [ ] Constant folding/propagation
21. [ ] Refactor lexing and parsing
22. [ ] Generate useful errors from initialization/declaration check
23. [ ] Make typechecker monadic

## Optional features
1. [ ] loop breaks

## Things to test
1. [ ] Am I allocating registers correctly? Should they not be freed?
