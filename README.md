## `microlex` is a small, yet effective lexical scanner.

*lex* has many odd conventions, because it mixes in *C* with its own psuedo-language,
This, among many other qualities, makes it difficult to use.

#### *microlex*, arguably, follows the Unix philosophy more than Lex:
- It is minimal:
	- It relies on only the OCaml standard library. No external fuss!
	- There is very little code. Only what is necessary is included. Compare that to the more-than-thousands of lines of code used for *flex* (a version of *lex* whose code can be found [here](https://github.com/westes/flex/tree/master/src)).
- You can use it how you need it (modularity):
	- If you want to embed *microlex* into your own program, call the `lex` function as needed. Otherwise, follow the example in the `demo` directory to read a file of lexemes with the `.mlx` extension. The Makefile details how you could embed such an example into your own use cases.

- Since it is magnitudes smaller than *flex*, and yet performs the same fundamental functions, it is likely that it runs faster than most *lex* variants like *flex*.
- Run `make` to get started.