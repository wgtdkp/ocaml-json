type json =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list

type state = {
  raw : string;
  cur : int;
  line : int;
}

exception Error of string

let rec next s =
  if s.cur >= String.length s.raw then
    raise (Error "premature end of file")
  else
    match s.raw.[s.cur] with
      | ' '
      | '\t'
      | '\r' -> next {raw=s.raw; cur=s.cur+1; line=s.line}
      | '\n' -> next {raw=s.raw; cur=s.cur+1; line=s.line+1}
      | _ -> ({raw=s.raw; cur=s.cur+1; line=s.line}, s.raw.[s.cur])

let test s c =
  let next_s, cur_c = next s in
  if cur_c = c then
    next_s, true
  else
    s, false

let expect s c =
  let s, b = test s c in
  if b then s else raise (Error "unexpected character")

let step s =
  if s.cur >= String.length s.raw then
    s, '\x00'
  else
    {raw=s.raw; cur=s.cur+1; line=s.line}, s.raw.[s.cur]

let rec parse_value s =
  let s, c = next s in
  match c with
    | '{' -> parse_object s []
    | '[' -> parse_array s []
    | '\"' -> Buffer.create 4 |> parse_string s
    | '0' .. '9' | '-' ->
        Buffer.create 4 |> fun buf -> Buffer.add_char buf c; parse_number s buf
    | 't' | 'f' -> c = 't' |> parse_bool s
    | 'n' -> parse_null s
    | _ -> raise (Error "unexpected character")

and parse_object s obj =
  let s, b = test s '}' in
  if b then s, Object obj else
    let s, v = parse_pair s in
    let s, b = test s  '}' in
    if b then s, Object (obj @ [v]) else
      let s = expect s ',' in
      let _, b = test s '}' in
      if b then raise (Error "trailing comma")
      else parse_object s (obj @ [v])

and parse_array s arr =
  let s, b = test s ']' in
  if b then s, Array arr else
    let s, v = parse_value s in
    let s, b = test s ']' in
    if b then s, Array (arr @ [v]) else
      let s = expect s ',' in
      let _, b = test s ']' in
      if b then raise (Error "trailing comma")
      else parse_array s (arr @ [v])

and parse_string s buf =
  let s, c = step s in
  match c with
    | '"' -> s, String (Buffer.contents buf)
    | '\\' ->
      (let s, c = step s in
       if c = 'u' then
          let s, uc = parse_utf8 s in
          let _ = Buffer.add_utf_8_uchar buf uc in
          parse_string s buf
        else
          let _ = match c with
            | '"' -> Buffer.add_char buf '\"'
            | '\\' -> Buffer.add_char buf '\\'
            | '/' -> Buffer.add_char buf '/'
            | 'b' -> Buffer.add_char buf '\b'
            | 'n' -> Buffer.add_char buf '\n'
            | 'r' -> Buffer.add_char buf '\r'
            | 't' -> Buffer.add_char buf '\t'
            | _ -> raise (Error "unexpected escpaed label")
          in parse_string s buf)
    | '\b' | '\n' | '\r' | '\t' ->
        raise (Error "unexpected control label")
    | '\x00' -> raise (Error "premature end of file")
    | _ -> (Buffer.add_char buf c; parse_string s buf)

and parse_utf8 s =
  if String.length s.raw < s.cur + 4 then
    raise (Error "invalid unicode")
  else
    let digits = String.sub s.raw s.cur 4 in
    let num = "0x" ^ digits |> int_of_string in
    if Uchar.is_valid num then
      s, Uchar.of_int num
    else
      raise (Error "invalid unicode")

and parse_number s buf =
  let next_s, c = step s in
  match c with
    | '0'..'9' | '.' | 'e' | 'E' | '-' | '+' ->
        (Buffer.add_char buf c; parse_number next_s buf)
    | _ ->
        try
          let str = Buffer.contents buf in
          if String.contains str '.' ||
            String.contains str 'e' ||
            String.contains str 'E' then
            s, Float (str |> float_of_string)
          else
            s, Int (str |> int_of_string)
        with Failure msg -> raise (Error msg)

and parse_bool s b =
  let prefix = if b then "rue" else "alse" in
  let len = min (String.length s.raw - s.cur) (String.length prefix) in
  if String.sub s.raw s.cur len |> String.equal prefix  then
    {raw=s.raw; cur=s.cur+len; line=s.line}, (Bool b)
  else raise (Error "expect true/false")

and parse_null s =
  let s = expect s 'u' in
  let s = expect s 'l' in
  let s = expect s 'l' in
  s, Null

and parse_pair s =
  let s = expect s '"' in
  let s, k = Buffer.create 4 |> parse_string s in
  let s = expect s ':' in
  let s, v = parse_value s in
  match k with
    | String k -> s, (k, v)
    | _ -> assert false

let from_string str =
  let s = {raw=str; cur=0; line=0} in
  let _, v = parse_value s in v

let from_channel ic =
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  from_string s

let from_file fname =
  let ic = open_in fname in
  from_channel ic
  
let rec pretty_print = function
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Array a -> "[" ^ (List.map pretty_print a |> String.concat ", ") ^ "]"
  | Object o -> "{" ^ (List.map print_pair o |> String.concat ", ") ^ "}"

(* OCaml cannot inference this polymorphism type. *)
(*
and print_list (printer:'a->string) (li:'a list) =
  List.map printer li |> String.concat ", "
*)

and print_pair (k, v) =
  pretty_print (String k) ^ ": " ^ pretty_print v
