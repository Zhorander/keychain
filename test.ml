open Encrypt
open Database

let test () =
  let keychain = open_db () in
  init();
  delete_service keychain "Test Service";
  let key = Digest.bytes "My secret key" in
  let msg = "Elias" in
  let e_msg = enc msg key in
  Printf.printf "encrypted key: %s\n" e_msg;
  let pt = dec e_msg key in
  Printf.printf "decrypted key: %s\n" pt;
  create_table keychain;
  create_entry "Test Service" (e_msg) "Test description" keychain;
  list_all keychain;
  if is_entry keychain "Test Service" then Printf.printf "Exists!\n" else Printf.printf "Doesn't exist:(\n";
  get_password keychain "Test Service" |> Printf.printf "password: %s\n";
  dec (get_password keychain "Test Service") key |> Printf.printf "decrypted pass: %s\n";
  close_db keychain;
  Printf.printf "Tetsts complete!\n";
  ;;

let () = test ()