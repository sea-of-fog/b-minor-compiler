type op = Add | Sub | Mul | Div

type expr =
    | NumberE of int
    | OpE of op * int * int
