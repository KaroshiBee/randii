type t = [
    `Error of string
  | `No_data
  | `Too_large of int
  | `Unknown_algo of string
  | `Unknown_digits of string
  | `Unknown_generator of string
  | `Unknown_word_size of string
]

let to_string = function
    `Error s -> "General error: " ^ s
  | `No_data -> "no data"
  | `Too_large n -> "Too large: " ^ (string_of_int n)
  | `Unknown_algo s -> "Unknown algo: " ^ s
  | `Unknown_digits s -> "Unknown digits: " ^ s
  | `Unknown_generator s -> "Unknown generator: " ^ s
  | `Unknown_word_size s -> "Unknown word size: " ^ s
