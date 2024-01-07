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

type 'a, 'b ann_expr =
    | NumAE   of 'a * int
    | OpAE    of 'a * op * expr * expr
    | VarAE   of 'a * 'b
    | AssAE   of 'a * string * expr
    | TrueAE  of 'a
    | FalseAE of 'a

type 'b, 'c ann_decl =
    | SimpADec of 'c * typ * ('b, 'c ann_expr)

(* annotated statements: variables for block data, expression data, memory representation *)
type 'a, 'b, 'c ann_stmt =
    | PrintS of 'b, 'c ann_expr 
    | DeclS  of 'c ann_decl
    | ExprS  of 'b, 'c expr 
    | BlockS of 'a * ('a, 'b, 'c stmt) list

type 'a, 'b, 'c ann_prog =
    'a, 'b, 'c ann_stmt list
    
type location =
    | Global of string
    | Local  of { scope : int;
                  pos   : int
                }
