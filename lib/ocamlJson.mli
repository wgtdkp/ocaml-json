open JsonType

exception Error of string

val from_string:
  string -> json

val from_channel:
  in_channel -> json

val from_file:
  string -> json

val pretty_print:
  json -> string
