module R = Randii.Rng

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

let process rng_name_arg () =
  let (let*) = Result.bind in
  let () = Logs.info (fun m -> m "rng with '%s'" rng_name_arg) in
  let key2 = [|"0";"1"|] in
  let key4 = [|"0";"1";"2";"3"|] in
  let ctr2 = [|"2";"3"|] in
  let ctr4 = [|"4";"5";"6";"7"|] in
  let p ~rng_name_arg () =
    let* {word_size; digits; algo} = R.RngName.of_string rng_name_arg in
    let* arr = match (word_size, digits, algo) with
      | R.Word_size.ThirtyTwo, R.Digits.Two, R.Algo.Threefry ->
        let r = R.R2x32.rand in
        let key = Array.map R.I32.of_string key2 in
        let ctr = Array.map R.I32.of_string ctr2 in
        r key ctr |> Array.map R.I32.to_string |> Result.ok
      | R.Word_size.ThirtyTwo, R.Digits.Four, R.Algo.Threefry ->
        let r = R.R4x32.rand in
        let key = Array.map R.I32.of_string key4 in
        let ctr = Array.map R.I32.of_string ctr4 in
        r key ctr |> Array.map R.I32.to_string |> Result.ok
      | R.Word_size.SixtyFour, R.Digits.Two, R.Algo.Threefry ->
        let r = R.R2x64.rand in
        let key = Array.map R.I64.of_string key2 in
        let ctr = Array.map R.I64.of_string ctr2 in
        r key ctr |> Array.map R.I64.to_string |> Result.ok
      | R.Word_size.SixtyFour, R.Digits.Four, R.Algo.Threefry ->
        let r = R.R4x64.rand in
        let key = Array.map R.I64.of_string key4 in
        let ctr = Array.map R.I64.of_string ctr4 in
        r key ctr |> Array.map R.I64.to_string |> Result.ok
    in
    Result.ok (Array.iter (fun s -> Printf.printf "\n%s" s) arr; Printf.printf "\n")
  in
  match p ~rng_name_arg () with
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

  let term (setup_log:unit Term.t) =
    Cmdliner.Term.(
      let t =
        (const process $ rng_name_arg $ setup_log)
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
