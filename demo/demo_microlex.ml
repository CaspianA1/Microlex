open Microlex

let rec print_strings lst =
	match lst with
		[] -> print_string "\n"
		| str :: rest ->
			Printf.printf "%s " str; print_strings rest

let matches = microlex "demo/ocaml_tokens.mlx" "demo/assorted_text.txt";;
print_match_data matches;;
print_endline "All tokens:";;
print_strings (extract_tokens matches)
(* Microlex just lexed some of itself! *)