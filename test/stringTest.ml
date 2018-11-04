open OUnit2
open OcamlJson

let test_basic _ =
  let json = from_string {| "tony stark" |} in
  match json with
    | String s -> print_string s
    | _ -> failwith ""
  (*assert_equal (String "tony stark") json*)

let test_escape _ =
  let json = from_string {| "\"tony \n stark\"" |} in
  assert_equal (String "\"tony \n stark\"") json

let test_error _ =
  assert_raises
    (Error "premature end of file")
    (fun _ -> from_string {| "tony stark |})

let _ =
  "string">:::[
    "basic">::test_basic;
    "negative">::test_escape;
    "error">::test_error
  ] |> run_test_tt_main
