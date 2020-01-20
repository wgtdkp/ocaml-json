{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let onenine = ['1'-'9']
let digit = '0' | onenine
let digits = digit+
let fraction = ('.' digits)?
let sign = ['+''-']?
let exponent = (['E''e'] sign digits)?
let ws = ['\x20''\x0A''\x0D''\x09']*
let integer = digit | onenine digits | '-' digit | '-' onenine digits
let number = integer fraction exponent
let hex = digit | ['A'-'F'] | ['a'-'f']
let escape = ['"''\\''/''b''f''n''r''t'] | 'u' hex hex hex hex

(* JSON character should in range ['0020'-'10FFFF'], see *)
(* https://www.json.org/json-en.html                     *)
let character = [^'"''\\'] | '\\' escape
let characters = character*
let string = '"' characters '"'

rule read =
  parse
  | ws { read lexbuf }
  | number {
      try
        INT (Lexing.lexeme lexbuf |> int_of_string)
      with _ ->
        FLOAT (Lexing.lexeme lexbuf |> float_of_string)
    }
  | "true" { TRUE }
  | "false" { FALSE }
  | "null" { NULL }
  | string {
      let str = Lexing.lexeme lexbuf in
      STRING (String.sub str 1 (String.length str - 2))
    }
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '[' { LEFT_BRACK }
  | ']' { RIGHT_BRACK }
  | ':' { COLON }
  | ',' { COMMA }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
