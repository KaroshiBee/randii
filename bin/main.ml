module T = Cbrn.Types

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


let process rng_name_arg key_arg ctr_arg n_arg kind_arg () =
  let (let*) = Result.bind in
  let kind = match kind_arg with
    | ("rand", None) -> T.Rand
    | ("uniform01", None) -> T.Uniform01
    | ("uniform", Some upper) -> T.Uniform upper
    | _ -> T.Rand
  in
  let n = max n_arg 1 in

  let () = Logs.info (fun m ->
      m "%d draws of '%s.%s' rng" n rng_name_arg
        (match kind with
         | Rand -> "rand"
         | Uniform01 -> "uniform01"
         | Uniform i -> "uniform "^(string_of_int i)))
  in
  let () = Logs.info (fun m -> m "command line key: [%s]" @@ String.concat "," key_arg) in
  let () = Logs.info (fun m -> m "command line ctr: [%s]" @@ String.concat "," ctr_arg) in

  let key_arg = if List.length key_arg > 0 then Some (Array.of_list key_arg) else None in
  let ctr_arg = if List.length ctr_arg > 0 then Some (Array.of_list ctr_arg) else None in

  let p () =
    let* arr = Cbrn.Rng.gen ~rng_name_arg ?key_arg ?ctr_arg n kind in
    Result.ok (Array.iter (fun s -> Printf.printf "\n%s" s) arr; Printf.printf "\n")
  in

  match p () with
  | Result.Ok () -> `Ok ()
  | Result.Error e -> `Error (true, (Cbrn.Errors.to_string e))

module Common = struct
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
      value & opt_all string [] & info ["k"] ~doc ~docv:"INT"
    )

  let ctr_arg =
    let doc =
      "The random generator starting counter (optional). \
       To be given in the form -c ctr1 -c ctr2 ...etc \
       depending on the digit size of the RNG you have specified.
      "
    in
    Arg.(
      value & opt_all string [] & info ["c"] ~doc ~docv:"INT"
    )

  let n_arg =
    let doc =
      "How many randoms to generate (optional). \
      "
    in
    Arg.(
      value & opt int 2 & info ["n"] ~doc ~docv:"INT"
    )
end

module Raw = struct
  open Cmdliner

  let term (setup_log:unit Term.t) =
    Cmdliner.Term.(
      let t = const process
              $ Common.rng_name_arg
              $ Common.key_arg
              $ Common.ctr_arg
              $ Common.n_arg
              $ const ("rand", None)
              $ setup_log
      in
      ret t
    )

  module Manpage = struct
    let command_description =
      "Generate raw random numbers - either 32bit or 64bit ints"

    let description = [`S "DESCRIPTION"; `P command_description]

    let man = description

    let info = Cmdliner.Cmd.info ~doc:command_description ~man "rand"
  end

  let cmd setup_log = Cmdliner.Cmd.v Manpage.info @@ term setup_log

end


module Uniform = struct
  open Cmdliner

  let uniform_arg =
    let doc =
      "A flag to specify the uniform distribution capped at an upper value (optional). \
      "
    in
    Arg.(
      value & opt int 10 & info ["upper"] ~doc ~docv:"INT"
    )

  let term (setup_log:unit Term.t) =
    Cmdliner.Term.(
      let make_uniform = app (const (fun upper -> ("uniform", Some upper))) in
      let t = const process
              $ Common.rng_name_arg
              $ Common.key_arg
              $ Common.ctr_arg
              $ Common.n_arg
              $ make_uniform uniform_arg
              $ setup_log
      in
      ret t
    )

  module Manpage = struct
    let command_description =
      "Generate uniform random numbers between [0, upper]"

    let description = [`S "DESCRIPTION"; `P command_description]

    let man = description

    let info = Cmdliner.Cmd.info ~doc:command_description ~man "uniform"
  end

  let cmd setup_log = Cmdliner.Cmd.v Manpage.info @@ term setup_log

end


module Uniform01 = struct
  open Cmdliner

  let term (setup_log:unit Term.t) =
    Cmdliner.Term.(
      let t = const process
              $ Common.rng_name_arg
              $ Common.key_arg
              $ Common.ctr_arg
              $ Common.n_arg
              $ const ("uniform01", None)
              $ setup_log
      in
      ret t
    )

  module Manpage = struct
    let command_description =
      "Generate uniform random floats between [0, 1]"

    let description = [`S "DESCRIPTION"; `P command_description]

    let man = description

    let info = Cmdliner.Cmd.info ~doc:command_description ~man "uniform01"
  end

  let cmd setup_log = Cmdliner.Cmd.v Manpage.info @@ term setup_log

end

(* group together all cmd lines *)
let commands = [
  Raw.cmd setup_log;
  Uniform.cmd setup_log;
  Uniform01.cmd setup_log;
]

let info =
  info ~doc:"The randii random number generator, \
             based on DEShaw Research 'Random123' \
             counter-based random number generator"
    "randii"

let main_cmd =
  Cmdliner.Cmd.group info commands

let () =
  exit (Cmdliner.Cmd.eval main_cmd)
