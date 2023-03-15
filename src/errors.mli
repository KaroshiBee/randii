type t = [
    `Error of string
  | `No_data
  | `Too_large of int
  | `Unknown_algo of string
  | `Unknown_digits of string
  | `Unknown_generator of string
  | `Unknown_word_size of string
]

val to_string : t -> string
