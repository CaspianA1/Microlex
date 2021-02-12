let rec keep_starting_inds indices =
	match indices with
		| [] -> []
		| index :: rest ->
			let tail = keep_starting_inds rest in
				match index with
					| [0; _] -> index :: tail
					| _ -> tail;;

let greediest_match str regexps =

	let match_length inds =
		match inds with
			| a :: [b] -> b - a
			| _ -> 0 in

	let rec _greediest_match str regexps greediest_regexp greediest_len =
		match regexps with
			| [] -> greediest_regexp
			| regexp :: rest ->
				let curr_len = match_length (match_ind regexp str) in
					if curr_len > greediest_len then
						_greediest_match str rest regexp curr_len
					else _greediest_match str rest greediest_regexp greediest_len in

	match regexps with
		| [] -> ""
		| regexp :: rest ->
			_greediest_match str rest regexp (match_length (match_ind regexp str));;

let rec print_list lst =
	match lst with
		[] -> print_string "\n"
		| x::xs -> print_int x; print_string " "; print_list xs ;;

let match_ind pattern str =
	let _ = search_forward (regexp pattern) str 0
		in [match_beginning (); match_end ()];;

let keep_starting_inds indices =
	List.filter
		(fun x -> match x with [0; _] -> true | _ -> false)
		indices;;

(*                *)

let slice lst start stop =
	let rec _slice lst counter start stop buffer =
		match lst with
			| [] -> []
			| head :: tail ->
				if counter = stop then append buffer [head]
				else _slice tail (counter + 1) start stop
					(if counter < start then buffer else append buffer [head]) in
	_slice lst 0 start stop [];;

let str_slice str start stop =
	let rec char_list str ind =
		if String.length str = ind then ""
		else (String.make 1 str.[ind]) ^ (char_list str (ind + 1))
	in slice (char_list str 0) start stop;; (* reduce to whole string *)

let a = str_slice "testing" 1 3;;
print_string a;;

(*
let add x y = x + y;;
let r = Stdlib.List.fold_right add [1; 2; 3] 0;;
print_int r;;
*)

(*
let to_char
let together_chars a b = String.make 1 a ^ String.make 1 b;;
let tog chrs = Stdlib.List.fold_right together_chars chrs ' ';;
*)

(*
match tokens with
	| [] -> None
	| token :: rest -> Some(_greediest_match str rest (first_match token str));;
*)

let print_match s_match =
	let internal_tok = s_match.tok in
		printf "Name = %s, pattern = %s, start = %d, stop = %d\n"
		internal_tok.name internal_tok.pattern s_match.start s_match.stop;;

	(*
	let test_string = "bobjohn099john900bob" and lexemes =
	[{name = "string"; pattern = "\".+\""};
	{name = "first_name"; pattern = "bob"};
	{name = "last_name"; pattern = "john"};
	{name = "number"; pattern = "[0-9]+"}] in
	let matches = lex test_string lexemes in print_matches test_string matches;;
	*)

(* Pretty printing: *)

let print_grammar grammar =
	let rec _print_grammar grammar ind =
		match grammar with
			| [regex; name] :: rest ->
				(printf "Lexeme #%d: regex = %s, name = %s\n" ind regex name;
				_print_grammar rest (ind + 1))
			| _ -> () in
	_print_grammar grammar 1;;

let rec print_lexemes lexemes =
	match lexemes with
		| [] -> ()
		| lexeme :: rest ->
			printf "Lexeme. name = %s, pattern = %s\n" lexeme.name lexeme.pattern;
			print_lexemes rest;;

let print_matches str matches =
	let rec _print_matches matches ind =
		match matches with
			| [] -> print_endline "---"
			| curr_match :: rest ->
				let ind_match = curr_match.the_match in
					let lex_unit = ind_match.lexical_unit in
						printf "---\nMatch #%d. `%s` was recognized from %d to %d.\n"
							ind curr_match.captured_str ind_match.start ind_match.stop;
						printf "The pattern of lexeme `%s` was `%s`.\n"
							lex_unit.name lex_unit.pattern;
		_print_matches rest (ind + 1) in
	printf "Text scanned: %s\n" str;
	_print_matches matches 1;;

(* Command line argument stuff: *)

let cmd_line_args () =
	let args = Sys.argv in
		let args_len = Array.length args - 1 in
		if args_len <> 2 then
			raise (Bad_arg_count (sprintf "Microlex expects 2 arguments, not %d" args_len))
		else [args.(1); args.(2)]

(* Greediness finder: *)

let greediest_match str tokens =
	let rec _greediest_match str tokens max =
		match tokens with
			| [] -> max
			| token :: rest ->
				_greediest_match str rest (
					match first_match token str with
						| Some(curr) ->
							if curr.stop - curr.start > max.stop - max.start && curr.start = 0
								then curr else max
						| None -> max) in
	_greediest_match str tokens
		{lexical_unit = {name = "None"; pattern = ""}; start = 0; stop = 0}

(* Some debugging notes: *)

(* print_strings (extract_tokens matches);; *)

(* I just need newlines *)

(*
open Str;;
open Printf;;

if (string_match (regexp "\n+") "\n" 0) then print_endline "Match!!!";;
*)

(*
Oddities:
Spaces
Newlines
Anything that needs to be escaped

Print the odd regexps to see what's up
*)

(* Other notes: *)

(*
Str.global_replace (Str.regexp "[\\n]") "\n" pattern
global_substitute (regexp "[\\.]") (sprintf "\%s")
*)

(*
All necessary convetsions:
( to \(

*)

(* Random: *)

(* let split_ind = search_forward (regexp "[^ ]+$") rule 0 in *)

(* (fun a b -> a ^ "\n" ^ b) (read_lines filename) "" in *)
(* (fun a b -> sprintf "%s\n%s" a b) (read_lines filename) "" in *)