# B-minor compiler

## Changes to the language
1. Declarations use let - that's easier to parse
2. Booleans are encoded as $0 or $(-1)

## Todo
1. [X] Generate code for print
2. [X] Booleans
3. [ ] Print booleans
4. [ ] Division operator
5. [ ] Boolean operators
6. [X] Add types to declarations
7. [ ] If/else
8. [ ] Uninitialized declarations
9. [ ] Check for initialized declarations
10. [ ] Typechecking/type inference
11. [ ] Loops
12. [ ] Arrays
13. [ ] Parsing whitespace
14. [ ] Functions
15. [ ] Constant folding/propagation
16. [ ] Refactor lexing and parsing
17. [ ] Generate useful errors from initialization/declaration check

## Optional features
1. [ ] loop breaks

## Things to test
1. [ ] Am I allocating registers correctly? Should they not be freed?
