open OUnit2
open OcamlJson

let test_basic _ =
  let json = from_string {| true |} in
  assert_equal (Bool true) json;
  let json = from_string {| false |} in
  assert_equal (Bool false) json

let test_error _ =
  assert_raises
    (Error "expect true/false")
    (fun _ -> from_string {| tru |});
  assert_raises
    (Error "expect true/false")
    (fun _ -> from_string {| fals |})

let _ =
  "bool">:::[
    "basic">::test_basic;
    "error">::test_error
  ] |> run_test_tt_main
