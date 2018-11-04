open OcamlJson

let () =
  try
    stdin |> from_channel |> pretty_print |> print_string;
  with Error msg -> prerr_string msg;;
  print_newline ()
