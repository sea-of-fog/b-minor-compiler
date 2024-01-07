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
1. [ ] Grammar: add annotations to AST
2. [ ] Division operator in codegen
3. [ ] Add modulo operator
4. [ ] Postfix operators
5. [ ] Add unit tests
6. [ ] Boolean not operator
7. [ ] Better assembly formatting
8. [ ] If/else
9. [ ] Elif
10. [ ] Uninitialised declarations
11. [ ] Check for initialised declarations
12. [ ] Loops
13. [ ] Arrays
14. [ ] Function primitives
15. [ ] Function declarations
16. [ ] Constant folding/propagation
17. [ ] Refactor lexing
18. [ ] Generate useful errors from initialization/declaration check
19. [ ] Make typechecker monadic
20. [ ] Comments
21. [ ] Code blocks
22. [ ] Change print statement to book version

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
10. [X] Tool: stop build process on error
11. [X] Grammar: make assingment an expression

## Optional features
1. [ ] loop breaks
2. [ ] function inlining

## Things to test
1. [X] Am I allocating registers correctly? Should they not be freed?

## Done
1. [X] Generate code for print
2. [X] Booleans
