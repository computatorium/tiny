type problem = {
  chapter : int;
  number : int;
  section : string;
  pc_id : string;
  uva_id : string;
  title : string;
}

exception Invalid of string

let invalid fmt = Printf.ksprintf (fun s -> raise (Invalid s)) fmt

(* The Problems subsection in each of the fourteen book chapters. *)
let subsections = [|6; 8; 8; 6; 9; 6; 6; 6; 6; 5; 6; 6; 6; 7|]

let natural label s =
  match int_of_string_opt s with
  | Some n when n > 0 && string_of_int n = s -> n
  | _ -> invalid "%s must be a canonical positive integer: %S" label s

let load path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let header = "chapter\tnumber\tsection\tpc_id\tuva_id\ttitle" in
    if (try input_line ic with End_of_file -> "") <> header then
      invalid "%s: expected header %S" path header;
    let seen = Hashtbl.create 512 in
    let unique kind value =
      let key = kind ^ ":" ^ value in
      if Hashtbl.mem seen key then invalid "duplicate %s: %s" kind value;
      Hashtbl.add seen key ()
    in
    let parse line =
      match String.split_on_char '\t' line with
      | [ch; num; section; pc_id; uva_id; title] ->
          let chapter = natural "chapter" ch and number = natural "number" num in
          if chapter > 14 || number > 8 then invalid "unknown problem %s/%s" ch num;
          let expected = Printf.sprintf "%d.%d.%d" chapter subsections.(chapter - 1) number in
          if section <> expected then invalid "section %S should be %S" section expected;
          let expected_pc = Printf.sprintf "11%02d%02d" chapter number in
          if pc_id <> expected_pc then invalid "PC ID %S should be %s" pc_id expected_pc;
          ignore (natural "UVa ID" uva_id);
          if title = "" || String.trim title <> title ||
             String.exists (fun c -> Char.code c < 32 || Char.code c = 127) title then
            invalid "invalid title %S" title;
          List.iter (fun (k,v) -> unique k v)
            ["identity", ch ^ "/" ^ num; "section", section; "PC ID", pc_id; "UVa ID", uva_id];
          {chapter; number; section; pc_id; uva_id; title}
      | _ -> invalid "expected six tab-separated fields"
    in
    let rec lines line_no acc =
      match input_line ic with
      | line ->
          let p = try parse line with Invalid s -> invalid "%s:%d: %s" path line_no s in
          lines (line_no + 1) (p :: acc)
      | exception End_of_file -> acc
    in
    let problems = lines 2 [] in
    if List.length problems <> 112 then invalid "%s: expected 112 problems, found %d" path (List.length problems);
    List.sort (fun a b -> compare (a.chapter,a.number) (b.chapter,b.number)) problems)

let generate ~root problems sources =
  let solvers = Hashtbl.create 112 in
  List.iter (fun path ->
    let prefix = root ^ "/" in
    if not (String.starts_with ~prefix path) then invalid "solution outside %s: %s" root path;
    let relative = String.sub path (String.length prefix) (String.length path - String.length prefix) in
    let chapter, number =
      match String.split_on_char '/' relative with
      | [ch; file] when String.length ch = 4 && String.length file = 6 &&
                        String.starts_with ~prefix:"ch" ch && String.starts_with ~prefix:"p" file &&
                        Filename.check_suffix file ".ml" ->
          (match int_of_string_opt (String.sub ch 2 2), int_of_string_opt (String.sub file 1 2) with
           | Some c, Some n when relative = Printf.sprintf "ch%02d/p%02d.ml" c n -> c,n
           | _ -> invalid "noncanonical solution path: %s (expected chNN/pNN.ml)" path)
      | _ -> invalid "noncanonical solution path: %s (expected chNN/pNN.ml)" path
    in
    if not (List.exists (fun p -> p.chapter = chapter && p.number = number) problems) then
      invalid "unknown problem in solution path: %s" path;
    if Hashtbl.mem solvers (chapter,number) then invalid "duplicate solution identity: %s" path;
    Hashtbl.add solvers (chapter,number) ()) sources;
  let b = Buffer.create 24000 in
  Buffer.add_string b "(* Generated from catalog/problems.tsv and solutions/**/*.ml. Do not edit. *)\nlet all : Pc.Harness.problem list = [\n";
  List.iter (fun p ->
    let solver = if Hashtbl.mem solvers (p.chapter,p.number) then
        Printf.sprintf "Some (Solutions.Ch%02d.P%02d.solve : Pc.Harness.solver)" p.chapter p.number
      else "None" in
    Printf.bprintf b "  { chapter = %d; number = %d; section = %S; pc_id = %S; uva_id = %S; title = %S; solver = %s };\n"
      p.chapter p.number p.section p.pc_id p.uva_id p.title solver) problems;
  Buffer.add_string b "]\n";
  Buffer.contents b
