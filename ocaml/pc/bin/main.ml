open Pc.Harness

let help = "\
Usage: pc list [--chapter N]\n\
       pc run ID\n\
       pc test [ID | --chapter N] [--tests-dir DIR] [--timeout SECONDS]\n\
       pc init ID [--case NAME] [--input FILE --output FILE] [--root DIR]\n\
       pc help\n\n\
IDs: book section (1.6.1), PC ID (110101 or pc:110101), UVa ID (uva:100).\n\
Fixtures: ./tests/<PC-ID>/<case>.in and <case>.out, compared byte for byte.\n\
Timeout: positive finite seconds per case; default 5.\n\
Init: creates a solution and sample fixture pair; keeps existing files.\n\
Exit codes: 0 success, 1 execution/judging failure, 2 invalid command/fixtures.\n"

let chapter value =
  match int_of_string_opt value with
  | Some n when n >= 1 && n <= 14 && string_of_int n = value -> n
  | _ -> invalid "chapter must be an integer from 1 to 14: %S" value

let filter ch problems = match ch with
  | None -> problems
  | Some n -> List.filter (fun p -> p.chapter = n) problems

let list problems =
  List.iter (fun p -> Printf.printf "%s  %-38s  pc:%s  uva:%s  %s\n"
    p.section p.title p.pc_id p.uva_id (if p.solver = None then "unimplemented" else "implemented")) problems;
  let implemented = List.length (List.filter (fun p -> p.solver <> None) problems) in
  Printf.printf "%d implemented · %d remaining\n" implemented (List.length problems - implemented);
  0

let parse_test args =
  let rec parse id ch root timeout = function
    | [] -> id,ch,Option.value root ~default:"./tests",Option.value timeout ~default:5.
    | "--chapter" :: n :: rest when ch = None && id = None -> parse id (Some (chapter n)) root timeout rest
    | "--tests-dir" :: dir :: rest when root = None && dir <> "" && not (String.starts_with ~prefix:"--" dir) ->
        parse id ch (Some dir) timeout rest
    | "--timeout" :: seconds :: rest when timeout = None ->
        let value = match float_of_string_opt seconds with
          | Some n when Float.is_finite n && n > 0. -> n
          | _ -> invalid "timeout must be positive finite seconds: %S" seconds in
        parse id ch root (Some value) rest
    | value :: rest when not (String.starts_with ~prefix:"-" value) && id = None && ch = None ->
        parse (Some value) ch root timeout rest
    | _ -> invalid "invalid test arguments (see pc help)"
  in parse None None None None args

let parse_init args =
  let rec parse id root case input output = function
    | [] ->
        let id = match id with Some id -> id | None -> invalid "init requires a problem ID (see pc help)" in
        id, Option.value root ~default:".", Option.value case ~default:"sample", input, output
    | "--root" :: dir :: rest when root = None && dir <> "" && not (String.starts_with ~prefix:"--" dir) ->
        parse id (Some dir) case input output rest
    | "--case" :: name :: rest when case = None -> parse id root (Some name) input output rest
    | "--input" :: file :: rest when input = None && not (String.starts_with ~prefix:"--" file) ->
        parse id root case (Some file) output rest
    | "--output" :: file :: rest when output = None && not (String.starts_with ~prefix:"--" file) ->
        parse id root case input (Some file) rest
    | value :: rest when id = None && not (String.starts_with ~prefix:"-" value) ->
        parse (Some value) root case input output rest
    | _ -> invalid "invalid init arguments (see pc help)"
  in parse None None None None None args

let main args =
  let all = Problem_registry.Registry.all in
  match args with
  | [] | ["help"] | ["--help"] | ["-h"] -> print_string help; 0
  | [("list" | "run" | "test" | "init"); ("--help" | "-h")] -> print_string help; 0
  | ["list"] -> list all
  | ["list"; "--chapter"; n] -> list (filter (Some (chapter n)) all)
  | ["run"; id] -> run (find all id)
  | "init" :: args ->
      let id,root,case,input,output = parse_init args in
      Pc.Bootstrap.init ~root ~case ~input ~output (find all id)
  | "test" :: args ->
      let id,ch,root,timeout = parse_test args in
      let selected = match id with Some id -> [find all id] | None -> filter ch all in
      test ~all ~selected ~specific:(id <> None) ~root ~timeout
        ~executable:(Unix.realpath Sys.executable_name)
  | _ -> invalid "invalid command (see pc help)"

let () =
  let code = try main (List.tl (Array.to_list Sys.argv)) with
    | Invalid s | Sys_error s -> Printf.eprintf "pc: %s\n" s; 2
    | Unix.Unix_error (e,fn,arg) -> Printf.eprintf "pc: %s(%s): %s\n" fn arg (Unix.error_message e); 2
  in exit code
