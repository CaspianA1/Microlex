open Str
open Printf
open List

let splitter s = search_forward (regexp "[^ ]+$") s 0

let tokenize_rule rule =
	let split_ind = search_forward (regexp "[^ ]+$") rule 0 in
		[string_before rule (split_ind - 1); string_after rule split_ind];;

let main () =
	let r = tokenize_rule "[a-zA-Z0-9]+@[a-zA-Z]+\\.com gmail_address" in
		printf "First = %s, second = %s\n" (nth r 0) (nth r 1);;

main ()