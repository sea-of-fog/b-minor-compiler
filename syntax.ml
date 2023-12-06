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
    | Int
    | Bool

type token =
    | Keyword of keyword
    | Id of string
    | Number of int
    | Op of op
    | OpenParen
    | ClosedParen
    | SemiColon
    | Colon
    | Equal

type typ =
    | IntT
    | BoolT

type expr =
    | NumE of int
    | OpE  of op * expr * expr
    | VarE of string
    | TrueE
    | FalseE

type decl =
    | SimpDec of string * typ * expr

type stmt =
    | AssS of string * expr
    | PrintS of expr

type instr =
    | Expr of expr
    | Decl of decl
    | Stmt of stmt

type prog = instr list
