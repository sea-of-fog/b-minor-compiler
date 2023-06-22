type op = Add | Sub | Mul | Div

type token =
    | Number of int
    | Op of op
    | OpenParen
    | ClosedParen

type expr =
    | NumE of int
    | OpE of op * int * int
