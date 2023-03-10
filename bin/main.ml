module R = Randii.Rng
(* module C = Randii.Ctr *)
(* module T = Randii.Threefry *)

(* module Ctr2x32 = C.Make_ctr(T.UInt32_2_T) *)
(* module Ctr2x64 = C.Make_ctr(T.UInt64_2_T) *)
(* module Ctr4x32 = C.Make_ctr(T.UInt32_4_T) *)
(* module Ctr4x64 = C.Make_ctr(T.UInt64_4_T) *)

module Threefry_2x32 = Randii.Rng.Threefry_2x32
module Threefry_2x64 = Randii.Rng.Threefry_2x64
module Threefry_4x32 = Randii.Rng.Threefry_4x32
module Threefry_4x64 = Randii.Rng.Threefry_4x64

(* a generic setup for logging *)
let setup_log =
  let init style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  Cmdliner.Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())


let info ~doc name =
  let version = Utils.Version.t in
  Cmdliner.Cmd.info
    ~doc
    ~version
    name

let process rng_name_arg key_arg ctr_arg n_arg () =
  let (let*) = Result.bind in
  let () = Logs.info (fun m -> m "%d draws of '%s' rng" n_arg rng_name_arg) in
  let () = Logs.info (fun m -> m "command line key: [%s]" @@ String.concat "," @@ List.map string_of_int key_arg) in
  let () = Logs.info (fun m -> m "command line ctr: [%s]" @@ String.concat "," @@ List.map string_of_int ctr_arg) in

  let key_arg = if List.length key_arg > 0 then Some (Array.of_list key_arg) else None in
  let ctr_arg = if List.length ctr_arg > 0 then Some (Array.of_list ctr_arg) else None in
  let n_arg = max n_arg 1 in

  (* default key/ctr values *)
  let key2 = [|0;1|] in
  let key4 = [|0;1;2;3|] in
  let ctr2 = [|0;1|] in
  let ctr4 = [|0;1;2;3|] in

  let p ~rng_name_arg ?key_arg ?ctr_arg n () =
    let* {word_size; digits; algo} = R.RngName.of_string rng_name_arg in
    let rand ?key_arg ?ctr_arg = function
      | R.Word_size.ThirtyTwo, R.Digits.Two, R.Algo.Threefry -> Threefry_2x32.(
          let* key = counter @@ Option.value key_arg ~default:key2 in
          let* ctr = counter @@ Option.value ctr_arg ~default:ctr2 in
          let* arr = rand ~key ~ctr in
          Result.ok (to_string_array arr, incr ctr |> to_string_array)
        )
      | R.Word_size.ThirtyTwo, R.Digits.Four, R.Algo.Threefry -> Threefry_4x32.(
          let* key = counter @@ Option.value key_arg ~default:key4 in
          let* ctr = counter @@ Option.value ctr_arg ~default:ctr4 in
          let* arr = rand ~key ~ctr in
          Result.ok (to_string_array arr, incr ctr |> to_string_array)
        )
      | R.Word_size.SixtyFour, R.Digits.Two, R.Algo.Threefry -> Threefry_2x64.(
          let* key = counter @@ Option.value key_arg ~default:key2 in
          let* ctr = counter @@ Option.value ctr_arg ~default:ctr2 in
          let* arr = rand ~key ~ctr in
          Result.ok (to_string_array arr, incr ctr |> to_string_array)
        )
      | R.Word_size.SixtyFour, R.Digits.Four, R.Algo.Threefry -> Threefry_4x64.(
          let* key = counter @@ Option.value key_arg ~default:key4 in
          let* ctr = counter @@ Option.value ctr_arg ~default:ctr4 in
          let* arr = rand ~key ~ctr in
          Result.ok (to_string_array arr, incr ctr |> to_string_array)
        )
    in
    let rec aux :
      ?key_arg:int array
      -> ?ctr_arg:int array
      -> int
      -> string array list
      -> (string array list, Randii.Errors.t) result
      = fun ?key_arg ?ctr_arg i acc ->
        if i > 0 then
          let* arr, ctr = rand ?key_arg ?ctr_arg (word_size, digits, algo) in
          let m = Array.length arr in
          match (word_size, digits, algo) with
          | R.Word_size.ThirtyTwo, R.Digits.Two, R.Algo.Threefry -> Threefry_2x32.(
              let* c = of_string_array ctr in
              let ctr_arg = Option.some @@ to_array c in
              aux ?key_arg ?ctr_arg (i-m) (arr :: acc)
            )
          | R.Word_size.ThirtyTwo, R.Digits.Four, R.Algo.Threefry -> Threefry_4x32.(
              let* c = of_string_array ctr in
              let ctr_arg = Option.some @@ to_array c in
              aux ?key_arg ?ctr_arg (i-m) (arr :: acc)
            )
          | R.Word_size.SixtyFour, R.Digits.Two, R.Algo.Threefry -> Threefry_2x64.(
              let* c = of_string_array ctr in
              let ctr_arg = Option.some @@ to_array c in
              aux ?key_arg ?ctr_arg (i-m) (arr :: acc)
            )
          | R.Word_size.SixtyFour, R.Digits.Four, R.Algo.Threefry -> Threefry_4x64.(
              let* c = of_string_array ctr in
              let ctr_arg = Option.some @@ to_array c in
              aux ?key_arg ?ctr_arg (i-m) (arr :: acc)
            )
        else
          Result.ok @@ List.rev acc
    in
    let* arr = aux ?key_arg ?ctr_arg n [] in
    let arr = Array.concat arr in
    let arr = Array.sub arr 0 n in
    Result.ok (Array.iter (fun s -> Printf.printf "\n%s" s) arr; Printf.printf "\n")
  in
  match p ~rng_name_arg ?key_arg ?ctr_arg n_arg () with
  | Result.Ok () -> `Ok ()
  | Result.Error e -> `Error (true, (Randii.Errors.to_string e))

module Term = struct
  open Cmdliner

  let rng_name_arg =
    let doc =
      "The random generator name (optional). \
       In the form nameNxBITS where \
       name is one of 'threefry', \
       N is 2 or 4 and BITS is 32 or 64"
    in
    Arg.(
      value & opt string "threefry4x64" & info ["r"; "rng"] ~doc ~docv:"NAME"
    )

  let key_arg =
    let doc =
      "The random generator starting key (optional). \
       To be given in the form -k key1 -k key2 ...etc \
       depending on the digit size of the RNG you have specified.
      "
    in
    Arg.(
      value & opt_all int [] & info ["k"] ~doc ~docv:"INT"
    )

  let ctr_arg =
    let doc =
      "The random generator starting ctr (optional). \
       To be given in the form -c ctr1 -c ctr2 ...etc \
       depending on the digit size of the RNG you have specified.
      "
    in
    Arg.(
      value & opt_all int [] & info ["c"] ~doc ~docv:"INT"
    )

  let n_arg =
    let doc =
      "How many randoms to generate (optional). \
      "
    in
    Arg.(
      value & opt int 2 & info ["n"] ~doc ~docv:"INT"
    )

  let term (setup_log:unit Term.t) =
    Cmdliner.Term.(
      let t =
        (const process $ rng_name_arg $ key_arg $ ctr_arg $ n_arg $ setup_log)
      in
      ret t
    )

end

module Manpage = struct
  let command_description =
    "Generate random numbers"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "gen"
end

let cmd setup_log = Cmdliner.Cmd.v Manpage.info @@ Term.term setup_log

(* group together all cmd lines *)
let commands = [
  cmd setup_log;
]

let info = info ~doc:"The randii random number generator" "randii"

let main_cmd =
  Cmdliner.Cmd.group info commands

let () =
  exit (Cmdliner.Cmd.eval main_cmd)
