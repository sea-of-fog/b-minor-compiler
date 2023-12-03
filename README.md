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
9. [ ] Testing infrastructure
10. [X] Add types to declarations
11. [ ] If/else
12. [ ] Uninitialised declarations
13. [ ] Check for initialised declarations
14. [ ] Loops
15. [ ] Arrays
16. [ ] Function primitives
17. [ ] Function declarations
18. [ ] Constant folding/propagation
19. [ ] Refactor lexing and parsing
20. [ ] Generate useful errors from initialization/declaration check
21. [ ] Make typechecker monadic

## Optional features
1. [ ] loop breaks

## Things to test
1. [ ] Am I allocating registers correctly? Should they not be freed?
