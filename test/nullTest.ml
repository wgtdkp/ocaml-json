open OUnit2
open OcamlJson

let test_basic _ =
  let json = from_string {| null |} in
  assert_equal Null json

let test_error _ =
  assert_raises
    (Error "premature end of file")
    (fun _ -> from_string {| nul |});
  assert_raises
    (Error "unexpected character")
    (fun _ -> from_string {| nil |})

let _ =
  "null">:::[
    "basic">::test_basic;
    "error">::test_error
  ] |> run_test_tt_main
