open Harness

let kind path =
  try Some (Unix.lstat path).Unix.st_kind
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let require_directory path =
  match kind path with
  | None | Some Unix.S_DIR -> ()
  | _ -> invalid "expected a directory: %s" path

let existing_file path =
  match kind path with
  | None -> false
  | Some Unix.S_REG -> true
  | _ -> invalid "expected a regular file: %s" path

let read path =
  if not (existing_file path) then invalid "sample file does not exist: %s" path;
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    really_input_string ic (in_channel_length ic))

let valid_case name =
  let alnum = function 'a'..'z' | 'A'..'Z' | '0'..'9' -> true | _ -> false in
  name <> "" && alnum name.[0] &&
  String.for_all (fun c -> alnum c || c = '-' || c = '_' || c = '.') name

let init ~root ~case ~input ~output p =
  if not (valid_case case) then
    invalid "invalid case name %S (start with a letter or digit; use letters, digits, -, _, or .)" case;
  let samples = match input, output with
    | None, None -> None
    | Some i, Some o -> Some (read i, read o)
    | _ -> invalid "--input and --output must be supplied together"
  in
  let root = Unix.realpath root in
  List.iter (fun file ->
    if not (existing_file (Filename.concat root file)) then
      invalid "%s is not a pc project root (use --root DIR)" root)
    ["dune-project"; "catalog/problems.tsv"; "solutions/dune"];
  let chapter = Printf.sprintf "solutions/ch%02d" p.chapter in
  let fixtures = "tests/" ^ p.pc_id in
  let dirs = ["solutions"; chapter; "tests"; fixtures] in
  let path file = Filename.concat root file in
  List.iter (fun dir -> require_directory (path dir)) dirs;
  let solution = Printf.sprintf "%s/p%02d.ml" chapter p.number in
  let input_file = fixtures ^ "/" ^ case ^ ".in" in
  let output_file = fixtures ^ "/" ^ case ^ ".out" in
  let template = Printf.sprintf
    "(* %s | pc:%s | uva:%s *)\nlet solve (_input : in_channel) (_output : out_channel) : unit =\n  failwith %S\n"
    p.section p.pc_id p.uva_id ("Not implemented: " ^ p.section ^ " - " ^ p.title) in
  let sample_input, sample_output = Option.value samples ~default:("", "") in
  let files = [solution, template; input_file, sample_input; output_file, sample_output] in
  let planned = List.map (fun (file, contents) -> file, contents, existing_file (path file)) files in
  if samples <> None then List.iter (fun (file, _, exists) ->
    if exists && file <> solution then invalid "fixture already exists: %s (choose another --case)" file) planned;
  let created = ref [] and made_dirs = ref [] in
  let cleanup f path = try f path with Sys_error _ | Unix.Unix_error _ -> () in
  (try
     List.iter (fun dir ->
       if kind (path dir) = None then begin
         Unix.mkdir (path dir) 0o755;
         made_dirs := path dir :: !made_dirs
       end) dirs;
     List.iter (fun (file, contents, exists) ->
       if not exists then begin
         (* Exclusive creation also protects edits made after the preflight. *)
         let oc = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o644 (path file) in
         created := path file :: !created;
         Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc contents; close_out oc)
       end) planned
   with exn ->
     List.iter (cleanup Sys.remove) !created;
     List.iter (cleanup Unix.rmdir) !made_dirs;
     raise exn);
  List.iter (fun (file, _, exists) -> Printf.printf "%s %s\n" (if exists then "Kept" else "Created") (path file)) planned;
  if samples = None && List.exists (fun (file, _, exists) -> file <> solution && not exists) planned then
    Printf.eprintf "Fill in %s/%s.in and .out before testing.\n" fixtures case;
  0
