type op = add | sub | mul | div

type token =
    | Number of int
    | Op of op
    | OpenParen
    | ClosedParen
