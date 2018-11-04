open OUnit2
open OcamlJson

let test_basic _ =
  let json = from_string {| ["tony", "stark"] |} in
  assert_equal (Array [String "tony"; String "stark"]) json

let test_empty _ =
  let json = from_string {| [] |} in
  assert_equal (Array []) json

let test_nested _ =
  let json = from_string {|
    [[1, 2, 3], [4, 5, 6]]
  |} in
  assert_equal
    (Array [(Array [(Int 1); (Int 2); (Int 3)]);
            (Array [(Int 4); (Int 5); (Int 6)])])
    json

let test_error _ =
  assert_raises
    (Error "trailing comma")
    (fun _ -> from_string {| [1, 2, 3,] |})

let _ =
  "array">:::[
    "basic">::test_basic;
    "empty">::test_empty;
    "nested">::test_nested;
    "error">::test_error
  ] |> run_test_tt_main
