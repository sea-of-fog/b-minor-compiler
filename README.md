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
8. [ ] Remove scoping opening/closing from typechecker
9. [ ] Better assembly formatting
10. [ ] Else
11. [ ] Elif
12. [ ] Uninitialised declarations
13. [ ] Check for initialised declarations
14. [ ] While loops
15. [ ] Do-while loops
16. [ ] For loops
17. [ ] Arrays
18. [ ] Function primitives
19. [ ] Function declarations
20. [ ] Constant folding/propagation
21. [ ] Refactor lexing
22. [ ] Generate useful errors from initialization/declaration check
23. [ ] Comments
24. [ ] Change print statement to book version
25. [ ] Unify interface for traversing AST with state; maybe Traversable from haskell?
26. [ ] Improve error messeages from type checking
27. [ ] Organize the project directory

## Bugs and issues

1. [X] Fix the weird issue of each scope opening twice
2. [X] Add scope stack back to AddressTable
3. [ ] Move max_temps etc. and the whole temp processing to the addressing module
4. [ ] Check all the partial matches
5. [ ] Make the state of label generators internal
6. [ ] Find out why parsing and lexing became so slow with the addition of comparison operators
6. [ ] Figure out how to make boolean comparison work

## Done
1. [X] Generate code for print
2. [X] Booleans
3. [X] Boolean operators in codegen
4. [X] Add interfaces to modules
5. [X] Typechecking/type inference
6. [X] Print booleans
7. [X] Parsing whitespace
8. [X] Testing infrastructure
9. [X] Fix bug in the compiler app
10. [X] Add types to declarations
11. [X] refactor parsing
12. [X] Tool: stop build process on error
13. [X] Grammar: make assingment an expression
14. [X] Code blocks
15. [X] Make typechecker monadic
16. [X] Grammar: add annotations to AST
17. [X] Division operator in codegen
18. [X] Implement temporary variables (for arbitrary length expressions)
19. [X] Comparison operators
20. [X] If

## Optional features
1. [ ] loop breaks
2. [ ] function inlining
3. [ ] static local variables

## Things to test
1. [X] Am I allocating registers correctly? Should they not be freed?
2. [ ] Check all tests (esp. 27) to see if variables are allocated correctly on the stack
