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
3. [ ] Division operator in codegen
4. [ ] Add modulo operator
5. [ ] Postfix operators
6. [ ] Add unit tests
7. [ ] If/else
8. [ ] Elif
9. [ ] Uninitialised declarations
10. [ ] Check for initialised declarations
11. [ ] Loops
12. [ ] Arrays
13. [ ] Function primitives
14. [ ] Function declarations
15. [ ] Constant folding/propagation
16. [ ] Refactor lexing
17. [ ] Generate useful errors from initialization/declaration check
18. [ ] Make typechecker monadic
19. [ ] Comments
20. [ ] Code blocks
21. [ ] Change print statement to book version

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
