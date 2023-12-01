type op = 
    | Add 
    | Sub 
    | Mul 
    | Div
    | Or
    | And

type keyword =
    | Let
    | Print
    | True
    | False

type token =
    | Keyword of keyword
    | Id of string
    | Number of int
    | Op of op
    | OpenParen
    | ClosedParen
    | Colon
    | Equal

type expr =
    | NumE of int
    | OpE  of op * expr * expr
    | VarE of string
    | TrueE
    | FalseE

type decl =
    | SimpDec of string * expr

type stmt =
    | AssS of string * expr
    | PrintS of expr

type instruction =
    | Expr of expr
    | Decl of decl
    | Stmt of stmt

type program = instruction list
