let latest_patch_version version =
  match
    List.find_opt
      (fun candidate ->
        Ocaml_version.equal version (Ocaml_version.without_patch candidate))
      Ocaml_version.Releases.recent
  with
  | Some x -> x
  | None -> failwith "Unknown ocaml version"

let latest_compiler_for_version version =
  let actual_version = latest_patch_version version in
  Ocaml_version.Opam.V2.name actual_version

module Let_syntax_cmdliner = struct
  open Cmdliner.Term

  let ( let+ ) t f = const f $ t

  let ( and+ ) a b = const (fun x y -> (x, y)) $ a $ b
end

let dry_run =
  let open Cmdliner.Arg in
  let info =
    info
      ~doc:
        "Do not perform external commands. Print them and continue as if they \
         worked."
      [ "dry-run" ]
  in
  value (flag info)

let run_fake cmd = Format.printf "Running: %a" Bos.Cmd.pp cmd

let run_real cmd = Bos.OS.Cmd.run cmd |> Rresult.R.failwith_error_msg

let runner =
  let open Let_syntax_cmdliner in
  let+ dry_run = dry_run in
  if dry_run then run_fake else run_real

let version =
  let open Cmdliner.Arg in
  let info = info ~doc:"Use this major OCaml version" [] in
  required (pos 0 (some string) None info)

let run runner version_string =
  let version = Ocaml_version.of_string_exn version_string in
  let cmd =
    Bos.Cmd.(
      v "opam" % "switch" % "create" % "./"
      % latest_compiler_for_version version
      % "--no-install")
  in
  runner cmd

let term =
  let open Let_syntax_cmdliner in
  let+ runner = runner and+ version = version in
  run runner version

let info = Cmdliner.Term.info "opam-autoswitch"

let run () =
  let result : unit Cmdliner.Term.result = Cmdliner.Term.eval (term, info) in
  Cmdliner.Term.exit result
