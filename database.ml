open Sqlite3

let linux_db_path = "./"
let linux_dir = "./"
let psswds_db = "psswds.db"
let table_name = "keyring"

(*temp hold encrypted password*)
(*Please wipe this with Cryptokit.wipe_string*)
(*When you're done with it*)
let temp_pass = ref ""

(*Set this to false when you are done with it*)
let bool_state = ref false

(*store the master key so user only has to type it once*)
(*Wipe this string before exiting*)
let master_key = ref "";;

let hash_str str =
  let open Cryptokit in
  let h_method = Hash.sha256 () in
  hash_string h_method str;;

let open_db () =
  db_open psswds_db;;

let close_db data_b =
  match db_close data_b with
  | true -> ();
  | false ->
    Printf.printf "Waiting for database to close\n";
    while db_close data_b do
    ();
    done;;

let delete_all data_b =
  match exec data_b ("delete from " ^table_name^";") with
  | _ -> ();;

let pad str =
  let len = Bytes.length str in
  let diff = 32 - len in
  let padding = Bytes.make diff '0' in
  str ^ padding;;
(* 
(*make way of entering passwords*)
let create_master_key () =
	Printf.printf "Master key has not been set.\nPlease enter a master key less than or equal to 32 characters\n::";
	let pass = read_line () in
	let pass = if (pass |> Bytes.length) > 32 then Bytes.sub pass 0 31 else pass |> pad in
	master_key := pass;
	let pass = hash_str pass in
	try
		let fd = Unix.openfile ("./"^"master.hsh") [Unix.O_TRUNC; Unix.O_WRONLY] 0o640 in
		let chann = Unix.out_channel_of_descr fd in
		Printf.fprintf chann "%s" pass;
	with _ -> Printf.printf "Error writing to master.hsh\n"; assert false;; *)

(*make directory where everything is stored*)
let make_dir () =
  let open Unix in
  Printf.printf "making dir...\n";
  try
    mkdir linux_dir 0o730;
  with Unix_error (a,b,c) ->
    a |> function
    | EEXIST -> Printf.printf "Done\n";
    | _ as a -> a |> error_message |> Printf.printf "%s\n"; exit 0;;

(*call this before anything else*)
let init () =
  let open Unix in
  let per = 0o640 (*rw-r-_*) in
  (* make_dir (); *)
  try
    let fd = openfile (linux_dir^psswds_db) [O_CREAT; O_RDWR] per in
    close fd;
  with
  | Unix_error (a,b,c) ->
    a |> function
    | EEXIST -> (); (*This means that a master psswd exists*)
    | _ as a -> a |> error_message |> Printf.printf "%s\n";;

(*Create the table*)
let create_table data_b =
  let status = exec data_b "create table keyring(
    Service text not null,
    Password text not null,
    Description text,
    Last_Modified text);"
  in
  match status with
  | Rc.OK -> (); (*Everything's ok!*)
  | Rc.ERROR -> ();
  | _ as a -> a |> Rc.to_string |> Printf.printf "Error in create_table: %s\n";;	

(*Need I say that psswd should be encrpyted*)
let create_entry serv psswd desc data_b =
  let open Unix in
  let lst_mod = time () |> localtime |> fun a ->
    let month = a.tm_mon + 1 |> string_of_int in
    let day = a.tm_mday |> string_of_int in
    let year = a.tm_year + 1900 |> string_of_int in
    month ^ "/" ^ day ^ "/" ^ year
  in                                  (*table params *)
  let entry = 
    Printf.sprintf "insert into keyring values (\"%s\", \"%s\", \"%s\", \"%s\");" serv psswd desc lst_mod
  in
  (* Printf.printf "%s\n" entry; *)
  match exec ~cb:(fun _ _ -> Printf.printf "???\n") data_b entry with
  | Rc.OK -> ();
  | _ as a -> a |> Rc.to_string |> Printf.printf "Error in create_entry: %s\n";;

let list_all data_b =
  let pretty_print hdr clmn =
    let serv = match hdr.(0) with Some a -> a; | None -> assert false in
    let des = match hdr.(2) with Some a -> a; | None -> "" in
    let lst = match hdr.(3) with Some a -> a; | None -> "" in
    Printf.printf "Service: %s\nDescription: %s\nLast_Modified: %s\n\n"
    serv des lst
  in
  match exec ~cb:pretty_print data_b "select * from keyring;" with
  | Rc.OK -> ();
  | _ as a -> a |> Rc.to_string |> Printf.printf "Error in list_all: %s\n";;

let search data_b serv =
  let pretty_print hdr clmn =
    let serv = match hdr.(0) with Some a -> a; | None -> assert false in
    let des = match hdr.(2) with Some a -> a; | None -> "" in
    let lst = match hdr.(3) with Some a -> a; | None -> "" in
    Printf.printf "Service: %s\nDescription: %s\nLast_Modified: %s\n\n"
    serv des lst
  in
  let sql_st = Printf.sprintf "select * from keyring where Service=\"%s\";" serv in
  match exec ~cb:pretty_print data_b sql_st with
  | Rc.OK -> ();
  | _ as a -> a |> Rc.to_string |> Printf.printf "Error in search or doesn't exist: %s\n";;

let is_entry data_b serv =
  bool_state := false;
  let sql_st = Printf.sprintf "select * from keyring where Service=\"%s\";" serv in
  match exec ~cb:(fun _ _ -> bool_state := true) data_b sql_st with
  | Rc.OK -> !bool_state;
  | _ -> false;;

let delete_service data_b serv =
  match is_entry data_b serv |> not with
  | true -> Printf.printf "Not a service\n";
  | false ->
    let sql_st = Printf.sprintf "delete from keyring where Service=\"%s\";" serv in
    match exec data_b sql_st with Rc.OK -> (); | _ -> Printf.printf "Error in delete_service\n";;

let get_password data_b serv =
  match is_entry data_b serv with
  | false -> Printf.printf "%s is not a service\n" serv; "";
  | true ->
    let set_pass hdr clmn =
      let pass = match hdr.(0) with Some a -> a; | None -> assert false in
      temp_pass := pass
    in
    let sql_st = Printf.sprintf "select Password from keyring where Service=\"%s\";" serv in
    match exec ~cb:set_pass data_b sql_st with Rc.OK -> !temp_pass; | _ -> Printf.printf "Error in get_password"; "";;
