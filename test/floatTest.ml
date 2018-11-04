open OUnit2
open OcamlJson

let test_basic _ =
  let json = from_string {| 123.0 |} in
  assert_equal (Float 123.0) json

let test_negative _ =
  let json = from_string {| -123.0 |} in
  assert_equal (Float (-123.0)) json

let test_e _ =
  let json = from_string {| 1.2e+4 |} in
  assert_equal (Float 12000.0) json;
  let json = from_string {| 1e-2 |} in
  assert_equal (Float 0.01) json

let test_error _ =
  assert_raises
    (Error "float_of_string")
    (fun _ -> from_string {| 1.2.3 |});
  assert_raises
    (Error "float_of_string")
    (fun _ -> from_string {| 1e2E3 |});
  assert_raises
    (Error "float_of_string")
    (fun _ -> from_string {| 0.1e.2 |})

let _ =
  "float">:::[
    "basic">::test_basic;
    "negative">::test_negative;
    "e">::test_e;
    "error">::test_error
  ] |> run_test_tt_main
