open Str
open String
open Scanf
open Printf

(*
let adjust_escapes pattern =
	let rec _adjust_escapes pattern curr_end =
		if pattern = "" then ""
		else
			try unescaped pattern
			with Scan_failure _ ->
				sprintf "%c%s" pattern.[0]
					(_adjust_escapes (sub pattern 1 curr_end) (curr_end - 1))

	in _adjust_escapes pattern ((length pattern) - 1);;

printf "Result = %s\n" (adjust_escapes "\nbob\\n(");;
*)

let main () =
	if string_match (regexp "\\(abc\\|def\\)") "abc" 0
		then print_endline "Yes, a match!"
	else print_endline "No match.";;

main ()