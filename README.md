# B-minor compiler

## Changes to the language
1. The compiler is named *bmc*
2. The extension for B- files is *.bm*
3. Declarations use let - that's easier to parse
4. Booleans are encoded as $0 or $(-1)

## Todo
3. [ ] Division operator in codegen
4. [ ] Add modulo operator
5. [X] Boolean operators in codegen
6. [X] Add interfaces to modules
7. [X] Typechecking/type inference
8. [X] Print booleans
9. [X] Parsing whitespace
10. [ ] Postfix operators
11. [ ] Add unit tests
12. [X] Testing infrastructure
13. [X] Fix bug in the compiler app
14. [X] Add types to declarations
15. [ ] If/else
16. [ ] Uninitialised declarations
17. [ ] Check for initialised declarations
18. [ ] Loops
19. [ ] Arrays
20. [ ] Function primitives
21. [ ] Function declarations
22. [ ] Constant folding/propagation
23. [ ] Refactor lexing and parsing
24. [ ] Generate useful errors from initialization/declaration check
25. [ ] Make typechecker monadic

## Optional features
1. [ ] loop breaks
2. [ ] function inlining

## Things to test
1. [X] Am I allocating registers correctly? Should they not be freed?

## Done
1. [X] Generate code for print
2. [X] Booleans
