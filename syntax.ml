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

type 'a ann_expr =
    | NumAE   of 'a * int
    | OpAE    of 'a * op * ('a ann_expr) * ('a ann_expr)
    | VarAE   of 'a 
    | AssAE   of 'a * ('a ann_expr)
    | TrueAE  of 'a 
    | FalseAE of 'a

type 'a ann_decl =
    | SimpADec of 'a * typ * ('a ann_expr)

type ('a, 'b) ann_stmt =
    | PrintAS of 'b ann_expr 
    | DeclAS  of 'b ann_decl
    | ExprAS  of 'b ann_expr 
    | BlockAS of 'a * (('a, 'b) ann_stmt) list

type ('a, 'b) ann_prog =
    ('a, 'b) ann_stmt list
    
type location =
    | Global of string
    | Local  of { scope : int;
                  pos   : int
                }
    | Temp

type block_data = 
    { name : string;
      local_vars : int }
