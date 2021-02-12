open Str
open String
open Printf
open Scanf

type lexeme = {name: string; pattern: string}
(* The start and stop here are relative to a constantly-shrinking string *)
type ind_match = {lexical_unit: lexeme; start: int; stop: int}
type match_data = {lexical_unit: lexeme; captured_str: string}

(* Begin IO stuff *)

exception Invalid_rule of string

let tokenize_mlx lines =
	let adjust_escapes pattern =
		let rec _adjust_escapes pattern curr_end =
			if pattern = "" then ""
			else
				try unescaped pattern
				with Scan_failure _ ->
					sprintf "%c%s" pattern.[0]
						(_adjust_escapes (sub pattern 1 curr_end) (curr_end - 1))
		in _adjust_escapes pattern ((length pattern) - 1) in

	let tokenize_rule rule ind =
		if contains (trim rule) ' ' then
			let split_ind = search_forward (regexp "[^ ]+\\( *\\)$") rule 0 in
				let correct_regex = adjust_escapes (string_before rule (split_ind - 1))
				and pattern_name = trim (string_after rule split_ind) in
				(* printf "Correct regex: %s\nPattern name: %s\n" correct_regex pattern_name; *)
				[correct_regex; pattern_name]

		else raise (Invalid_rule (sprintf "Rule #%d was not given a name." ind)) in

	let rec _tokenize_mlx lines ind =
		match lines with
			| [] -> []
			| line :: rest ->
				tokenize_rule line ind :: _tokenize_mlx rest (ind + 1) in
	_tokenize_mlx lines 1

let read_lines filename =
	let rec _read_lines file_obj =
		try let next_line = input_line file_obj in
			next_line :: _read_lines file_obj
		with End_of_file -> [] in

	let file_obj = open_in filename in
		let lines = _read_lines file_obj in
			close_in file_obj;
			lines

let file_contents filename =
	let contents = Stdlib.List.fold_right
		(sprintf "%s\n%s") (read_lines filename) "" in
	sub contents 0 ((length contents) - 1)

let rec print_match_data matches =
	match matches with
		| [] -> ()
		| curr_match :: rest ->
			printf "The lexeme with name '%s' and pattern '%s' matched '%s'.\n"
			curr_match.lexical_unit.name
			(escaped curr_match.lexical_unit.pattern) curr_match.captured_str;
			print_match_data rest

(* End IO stuff, begin lexing stuff *)

exception Invalid_pattern of string

let first_match token str =
	try let regex = regexp token.pattern in
		(try let _ = search_forward regex str 0 in
				Some({lexical_unit = token; start = match_beginning (); stop = match_end ()})
			with Not_found -> None)
	with Failure _ -> raise (Invalid_pattern (sprintf "Malformed pattern: '%s'." token.pattern))

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

let rec lex str lexemes =
	let curr_match = greediest_match str lexemes in
		if curr_match.lexical_unit.name = "None" then
			(eprintf "No tokens match this text: '%s'.\n" str; [])
		else let strlen = length str in
			if strlen = curr_match.stop then
				[{lexical_unit = curr_match.lexical_unit; captured_str = str}]
			else
				let after_token = sub str curr_match.stop (strlen - curr_match.stop)
				and lex_datum = {lexical_unit = curr_match.lexical_unit;
								captured_str = sub str 0 curr_match.stop} in
					lex_datum :: lex after_token lexemes

let rec make_lexeme_table lexemes =
	match lexemes with
		| [regex; name] :: rest ->
			{name = name; pattern = regex} :: make_lexeme_table rest
		| _ -> []

let rec extract_tokens matches =
	match matches with
		| [] -> []
		| curr :: rest -> curr.lexical_unit.name :: extract_tokens rest;;

let microlex mlx_file text_file =
	let lexemes = make_lexeme_table (tokenize_mlx (read_lines mlx_file)) in
		lex (file_contents text_file) lexemes