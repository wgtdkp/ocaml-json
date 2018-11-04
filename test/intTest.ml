open OUnit2
open OcamlJson

let test_basic _ =
  let json = from_string {|123|} in
  assert_equal (Int 123) json

let test_negative _ =
  let json = from_string {| -123 |} in
  assert_equal (Int (-123)) json

let test_error _ =
  assert_raises
    (Error "unexpected character")
    (fun _ -> from_string {| +123 |})

let _ =
  "int">:::[
    "basic">::test_basic;
    "negative">::test_negative;
    "error">::test_error
  ] |> run_test_tt_main
