type json =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list

exception Error of string

val from_string:
  string -> json

val from_channel:
  in_channel -> json

val from_file:
  string -> json

val pretty_print:
  json -> string
