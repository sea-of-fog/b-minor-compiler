# B-minor compiler

## Changes to the language
1. Declarations use let - that's easier to parse
2. Booleans are encoded as $0 or $(-1)

## Todo
1. [X] Generate code for print
2. [X] Booleans
3. [ ] Division operator in codegen
5. [ ] Boolean operators in codegen
6. [ ] Add interfaces to modules
7. [ ] Typechecking/type inference
8. [ ] Print booleans
9. [ ] Parsing whitespace
10. [ ] Testing infrastructure
11. [X] Add types to declarations
12. [ ] If/else
13. [ ] Uninitialised declarations
14. [ ] Check for initialised declarations
15. [ ] Loops
16. [ ] Arrays
17. [ ] Functions
18. [ ] Constant folding/propagation
19. [ ] Refactor lexing and parsing
20. [ ] Generate useful errors from initialization/declaration check

## Optional features
1. [ ] loop breaks

## Things to test
1. [ ] Am I allocating registers correctly? Should they not be freed?
