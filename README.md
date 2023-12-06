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
9. [ ] Postfix operators
10. [ ] Add unit tests
11. [X] Testing infrastructure
12. [X] Fix bug in the compiler app
13. [X] Add types to declarations
14. [ ] If/else
15. [ ] Uninitialised declarations
16. [ ] Check for initialised declarations
17. [ ] Loops
18. [ ] Arrays
19. [ ] Function primitives
20. [ ] Function declarations
21. [ ] Constant folding/propagation
22. [ ] Refactor lexing and parsing
23. [ ] Generate useful errors from initialization/declaration check
24. [ ] Make typechecker monadic

## Optional features
1. [ ] loop breaks
2. [ ] function inlining

## Things to test
1. [ ] Am I allocating registers correctly? Should they not be freed?
