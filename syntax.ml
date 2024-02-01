type op = 
    | Add 
    | Sub 
    | Mul 
    | Div
    | Or
    | And
    | Eq
    | Neq
    | Lt
    | Leq
    | Gt
    | Geq

type keyword =
    | Let
    | Print
    | True
    | False
    | Int
    | Bool
    | If

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
    | PrintS of expr
    | DeclS  of decl
    | ExprS  of expr 
    | IfS    of stmt
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
    | IfAS    of ('a, 'b) ann_stmt

type ('a, 'b) ann_prog =
    ('a, 'b) ann_stmt list
    
type location =
    | GlobalLoc of string
    | LocalLoc  of { scope : int;
                  pos   : int
                }
    | TempLoc

type block_data = 
    { label : string;
      local_vars : int }

type block_data_v2 = {
    label_v2      : string;
    local_vars_v2 : int;
    temp_vars_v2  : int;
    }

type register =
    | RBX
    | R09
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15

type memory =
    | GlobalMem    of string
    | RegisterMem  of register
    | LocalMem     of int
    | TempMem      of int
