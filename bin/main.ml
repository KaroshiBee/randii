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
  let p
      ~rng_name_arg:_
      () =
    (* let rng_name = R.RngName.of_string *)
    `Ok ()
  in
  p ~rng_name_arg ()

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
