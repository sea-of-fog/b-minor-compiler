# B-minor compiler

## Changes to the language
1. The compiler is named *bmc*
2. The extension for B- files is *.bm*
3. Declarations use let - that's easier to parse
4. Booleans are encoded as $0 or $(-1)

## Register policy on X86_64
When generating expressions, rbx and r9 are ALWAYS kept free,
only r10-r15 are used for scratch.

## Todo
1. [ ] Grammar: make assingment an expression
2. [ ] Grammar: add annotations to AST
3. [ ] Helper: stop build process on error
4. [ ] Division operator in codegen
5. [ ] Add modulo operator
6. [ ] Postfix operators
7. [ ] Add unit tests
8. [ ] Boolean not operator
9. [ ] If/else
10. [ ] Elif
11. [ ] Uninitialised declarations
12. [ ] Check for initialised declarations
13. [ ] Loops
14. [ ] Arrays
15. [ ] Function primitives
16. [ ] Function declarations
17. [ ] Constant folding/propagation
18. [ ] Refactor lexing
19. [ ] Generate useful errors from initialization/declaration check
20. [ ] Make typechecker monadic
21. [ ] Comments
22. [ ] Code blocks
23. [ ] Change print statement to book version

## Done
1. [X] Boolean operators in codegen
2. [X] Add interfaces to modules
3. [X] Typechecking/type inference
4. [X] Print booleans
5. [X] Parsing whitespace
6. [X] Testing infrastructure
7. [X] Fix bug in the compiler app
8. [X] Add types to declarations
9. [X] refactor parsing

## Optional features
1. [ ] loop breaks
2. [ ] function inlining

## Things to test
1. [X] Am I allocating registers correctly? Should they not be freed?

## Done
1. [X] Generate code for print
2. [X] Booleans
