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
    | OpenCurly
    | ClosedCurly
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
    | AssE of string * expr
    | TrueE
    | FalseE

type decl =
    | SimpDec of string * typ * expr

type stmt =
    | PrintS of expr * (typ option)
    | DeclS  of decl
    | ExprS  of expr 
    | BlockS of stmt list

type prog = stmt list
