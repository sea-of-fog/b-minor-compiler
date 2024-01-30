# B-minor compiler

## Changes to the language
1. The compiler is named bmc*
2. The extension for B- files is *.bm*
3. Declarations use let - that's easier to parse
4. Booleans are encoded as $0 or $(-1)

## Register policy on X86_64
When generating expressions, rbx and r9 are ALWAYS kept free,
only r10-r15 are used for scratch.

## Scoping
Each code block allocates its own local variables on the stack. Therefore, the stack can change during one function call. The number of local variables is the same for the whole block.

## Todo
1. [ ] Erase the "type" parameter from declaration - it'll have it anyway!
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
23. [ ] Unify interface for traversing ast with state; maybe Traversable from haskell?
24. [ ] Improve error messeages from type checking

## Bugs and issues

1. [ ] Fix the weird issue of each scope opening twice
2. [ ] Add scope stack back to AddressTable

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
15. [X] Division operator in codegen
16. [X] Implement temporary variables (for arbitrary length expressions)

## Optional features
1. [ ] loop breaks
2. [ ] function inlining
3. [ ] static local variables

## Things to test
1. [X] Am I allocating registers correctly? Should they not be freed?

## Done
1. [X] Generate code for print
2. [X] Booleans
