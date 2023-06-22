(* This version of the parser is based on the following CFG:
 * S -> E (as in "expression")
 * E -> E + T | E - T | T (as in "term")
 * T -> N | (E) (N stands for "number")
 * 
 * A few notes:
 * 1) here, we are operating on tokens, so whole numbers are 
 *    lexical nonterminals
 * 2) this grammar has left-recursive productions, so is not suitable
 *    for automatically generating parsers
 * 3) however, any valid parsing tree IS LEFT-ASSOCIATIVE
 * 
 * The implementation will be based on an alternative, right-recursive
 * CFG. The trick for eliminating left-recursion can be found in: 
 * (slides 3/4)
 * https://people.montefiore.uliege.be/geurts/Cours/compil/2011/03-syntaxanalysis-2.pdf
 *
 * New:
 * S  -> E
 * E  -> TE'
 * E' -> +TE' | -TE' | \varepsilon
 * T  -> AT'
 * T' -> *AT' | /AT' | \varepsilon
 * A -> Number | (E)
 *
 * Notes on implementation: 
 * 1) since we are building left-associative expressions,
 *    the parsers for T' and S' will return lambdas waiting for the right term
 * 2) Numbers are terminals, as we are working with the results of lexical analysis
 *)
