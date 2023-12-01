let source_file = Sys.argv.(1)
let target_file = Sys.argv.(2)

let rec output_lines channel lines =
    match lines with
    | [] -> ()
    | line::lines -> let () = O.output_string channel ("\t"^line^"\n")
                     in output_lines channel lines

module O = Out_channel

let read_whole_file filename =
    let ch = open_in_bin filename in
        let s = really_input_string ch (in_channel_length ch) in
            close_in ch; s

let source_code = read_whole_file source_file

let ts = Lexer.main_lex source_code
let prog = Parser.program_parser ts

let () = if Analyser.check_if_declared prog
         then ()
         else print_endline "There are undeclared variables"

let target_code = Codegen.program_codegen prog

let () = let channel = O.open_text target_file in 
            output_lines channel target_code
