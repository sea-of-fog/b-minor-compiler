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
1. [ ] Division operator in codegen
2. [ ] Negative numbers!
3. [ ] Add modulo operator
4. [ ] Postfix operators
5. [ ] Add unit tests
6. [ ] Boolean not operator
7. [ ] Comparison operators
8. [ ] Remove scoping opening/closing from typechecker
9. [ ] Better assembly formatting
10. [ ] If/else
11. [ ] Elif
12. [ ] Uninitialised declarations
13. [ ] Check for initialised declarations
14. [ ] Loops
15. [ ] Arrays
16. [ ] Function primitives
17. [ ] Function declarations
18. [ ] Constant folding/propagation
19. [ ] Refactor lexing
20. [ ] Generate useful errors from initialization/declaration check
21. [ ] Comments
22. [ ] Change print statement to book version
23. [ ] Implement temporary variables (for arbitrary length expressions)
24. [ ] Unify interface for traversing ast with state; maybe Traversable from haskell?

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
12. [X] Code blocks
13. [X] Make typechecker monadic
14. [X] Grammar: add annotations to AST

## Optional features
1. [ ] loop breaks
2. [ ] function inlining

## Things to test
1. [X] Am I allocating registers correctly? Should they not be freed?

## Done
1. [X] Generate code for print
2. [X] Booleans
