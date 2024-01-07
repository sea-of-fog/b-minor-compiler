let source_file = 
    Sys.argv.(1)
let output_file = 
    Sys.argv.(2)

let rec output_lines channel lines =
    match lines with
    | [] -> ()
    | line::lines -> let () = Out_channel.output_string channel ("\t"^line^"\n")
                     in output_lines channel lines

let read_whole_file filename =
    let ch = open_in_bin filename in
        let s = really_input_string ch (in_channel_length ch) in
            close_in ch; s

let source_code = 
    read_whole_file source_file

let ts = 
    Scanner.main_lex source_code

let prog = 
    Parsing.program_parser ts

let scoped_prog =
    match Scope.resolve prog with
    | ScopeTable.Ok scoped_prog ->
        scoped_prog
    | ScopeTable.Fail msg ->
        failwith msg

let typed_prog =
    match Type.check scoped_prog with
    | TypeTable.Ok typed_prog ->
        typed_prog
    | TypeTable.Fail msg ->
        failwith "msg"

let target_code = X86Codegen.program_codegen typed_prog

let () = let channel = Out_channel.open_text output_file in 
    output_lines channel target_code; Out_channel.close channel
