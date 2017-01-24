open Encrypt
open Database

let truncate str len =
  if (str |> Bytes.length) > len then Bytes.sub str 0 (len - 1) else str;;

let bracket before after f init =
	let x = before init in
	let res =
		try f x with exn -> after x; raise exn
	in
	after x;
	res;;

let prompt ?(echo=true) s =
	output_string stdout s;
	flush stdout;
	if echo then
		input_line stdin
	else
		let fd = Unix.descr_of_in_channel stdin in
		let tio = Unix.tcgetattr fd in
		let old_echo = tio.Unix.c_echo in
		bracket
			(fun () ->
			tio.Unix.c_echo <- false;
			Unix.tcsetattr fd Unix.TCSADRAIN tio)
			(fun () ->
			tio.Unix.c_echo <- old_echo;
			Unix.tcsetattr fd Unix.TCSADRAIN tio;
			output_char stdout '\n';
			flush stdout)
			(fun _ ->
			input_line stdin
			) ()

let get_master () =
	let k = (prompt ~echo:false "Enter your master key:" |> truncate) 32 |> pad in
	master_key := k;;

let main () =
	let open Printf in
	init ();
	let keychain = open_db () in
	get_master ();
	while true do
		printf "Enter Choice\n1. Enter Service\n2. Delete Service\n3. Print Services\n4. Get Password\n5. Delete All Entries\n6. Search Service\n7. Clear Screen\n8. Exit\n::";
		match read_line () |> int_of_string with
		| 1 ->
			printf "Enter name of service:";
			let serv = (read_line () |> truncate) 32 in
			printf "Enter password for service:";
			let pass = (read_line () |> enc) !master_key in
			printf "Enter description of service:";
			let desc = (read_line () |> truncate) 64 in
			create_entry serv pass desc keychain;
		| 2 ->
			printf "Enter name of service (q to go back):";
			let serv = (read_line () |> truncate) 32 in
			if serv <> "q" then delete_service keychain serv;
		| 3 -> list_all keychain;
		| 4 ->
			printf "Enter name of service:";
			let serv = (read_line () |> truncate) 32 in
			let pass = get_password keychain serv in
			printf "Password is:%s\n" ((pass |> dec) !master_key);
		| 5 ->
			printf "Are you sure?(y/n):";
			let resp = (read_line () |> Bytes.sub) 0 1 in
			if resp = "y" then delete_all keychain;
		| 6 ->
			printf "Enter a service(q to quit):";
			let serv = (read_line () |> truncate) 32 in
			begin
			match serv with
			| "q" -> ();
			| _ as serv -> if is_entry keychain serv then printf "Exists!\n" else printf "Doesn't Exist\n";
			end;
		| 7 -> for i = 0 to 200 do printf "\n"; done;
		| 8 ->
			wipe_str !master_key; 
			exit 0;
		| _ -> printf "Not a valid choice\n";
	done;;



let () = main ();;