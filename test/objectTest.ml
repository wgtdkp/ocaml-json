open OUnit2
open OcamlJson

let test_basic _ =
  let json = from_string {|
    {"name": "OcamlJson"}
  |} in
  assert_equal (Object [("name", String "OcamlJson")]) json

let test_empty _ =
  let json = from_string {|
    {}
  |} in
  assert_equal (Object []) json

let test_nested _ =
  let json = from_string {|
    {
      "friend": {
        "name": "tony",
        "age": 35},
      "father": {
        "name": "stark",
        "age": 55
      }
    }
  |} in
  assert_equal
    (Object [("friend", (Object [("name", String "tony"); ("age", Int 35)]));
             ("father", (Object [("name", String "stark"); ("age", Int 55)]))])
    json

let test_error _ =
  assert_raises
    (Error "trailing comma")
    (fun _ -> from_string {| {"name": "tony",} |})

let _ =
  "object">:::[
    "basic">::test_basic;
    "empty">::test_empty;
    "nested">::test_nested;
    "error">::test_error
  ] |> run_test_tt_main
