type solver = in_channel -> out_channel -> unit

type problem = {
  chapter : int;
  number : int;
  section : string;
  pc_id : string;
  uva_id : string;
  title : string;
  solver : solver option;
}

exception Invalid of string
let invalid fmt = Printf.ksprintf (fun s -> raise (Invalid s)) fmt

let find problems id =
  match List.find_opt (fun p ->
    id = p.section || id = p.pc_id || id = "pc:" ^ p.pc_id || id = "uva:" ^ p.uva_id) problems with
  | Some p -> p
  | None -> invalid "unknown problem %S (use a book section, PC ID, pc:ID, or uva:ID)" id

let run p =
  match p.solver with
  | None -> Printf.eprintf "pc: %s (%s) is unimplemented\n%!" p.section p.title; 1
  | Some solve ->
      try
        set_binary_mode_in stdin true;
        set_binary_mode_out stdout true;
        solve stdin stdout;
        flush stdout;
        0
      with exn -> Printf.eprintf "pc: %s: %s\n%!" p.section (Printexc.to_string exn); 1

type fixture = { name : string; input : string; expected : string }

let entries dir = Sys.readdir dir |> Array.to_list |> List.sort String.compare
let kind path = (Unix.lstat path).Unix.st_kind

let discover problems root =
  if kind root <> Unix.S_DIR then invalid "fixture root is not a directory: %s" root;
  let fixtures = Hashtbl.create 112 in
  List.iter (fun id ->
    if id <> ".gitkeep" then begin
      let dir = Filename.concat root id in
      if not (List.exists (fun p -> p.pc_id = id) problems) then
        invalid "unknown fixture PC ID: %s" id;
      if kind dir <> Unix.S_DIR then invalid "expected fixture directory: %s" dir;
      let cases = Hashtbl.create 16 in
      List.iter (fun file ->
        if file <> ".gitkeep" then begin
          let path = Filename.concat dir file in
          if kind path <> Unix.S_REG then invalid "expected a regular fixture file: %s" path;
          let suffix =
            if Filename.check_suffix file ".in" then ".in"
            else if Filename.check_suffix file ".out" then ".out"
            else invalid "unexpected fixture file: %s (expected CASE.in or CASE.out)" path
          in
          let name = Filename.chop_suffix file suffix in
          if name = "" then invalid "empty fixture case name: %s" path;
          let input, output = Option.value (Hashtbl.find_opt cases name) ~default:(false,false) in
          Hashtbl.replace cases name (input || suffix = ".in", output || suffix = ".out")
        end) (entries dir);
      let pairs = Hashtbl.fold (fun name (input,output) acc ->
        if not (input && output) then invalid "unmatched fixture: %s/%s (need both .in and .out)" dir name;
        {name; input = Filename.concat dir (name ^ ".in");
         expected = Filename.concat dir (name ^ ".out")} :: acc) cases [] in
      Hashtbl.add fixtures id (List.sort (fun a b -> String.compare a.name b.name) pairs)
    end) (entries root);
  fixtures

type difference = { offset : int; expected : string; actual : string }

let compare_files expected actual =
  let a = open_in_bin expected in
  Fun.protect ~finally:(fun () -> close_in_noerr a) (fun () ->
    let b = open_in_bin actual in
    Fun.protect ~finally:(fun () -> close_in_noerr b) (fun () ->
      let byte ic = try Some (input_char ic) with End_of_file -> None in
      let rec first offset =
        match byte a, byte b with
        | None, None -> None
        | x, y when x = y -> first (offset + 1)
        | _ -> Some offset
      in
      match first 0 with
      | None -> None
      | Some offset ->
          let context ic =
            let start = max 0 (offset - 20) in
            let len = in_channel_length ic in
            let start = min start len in
            seek_in ic start;
            let s = really_input_string ic (min 61 (len - start)) in
            Printf.sprintf "bytes %d..: %S%s" start s (if offset >= len then " <EOF>" else "")
          in
          Some {offset; expected = context a; actual = context b}))

let excerpt path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let len = in_channel_length ic in
    really_input_string ic (min 4096 len) ^ (if len > 4096 then "\n[stderr truncated]" else ""))

type execution = Finished of Unix.process_status | Timed_out

let kill_group pid =
  try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error (Unix.ESRCH,_,_) -> ()

let rec reap flags pid =
  try Unix.waitpid flags pid with Unix.Unix_error (Unix.EINTR,_,_) -> reap flags pid

(* File-backed capture avoids pipe deadlocks when a solver writes large outputs.
   Each child creates a session so timeout cleanup also reaches its descendants. *)
let execute ~executable ~timeout p fixture output errors =
  let input_fd = Unix.openfile fixture.input [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close input_fd) (fun () ->
    let output_fd = Unix.openfile output [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CLOEXEC] 0o600 in
    Fun.protect ~finally:(fun () -> Unix.close output_fd) (fun () ->
      let error_fd = Unix.openfile errors [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CLOEXEC] 0o600 in
      Fun.protect ~finally:(fun () -> Unix.close error_fd) (fun () ->
        flush_all ();
        match Unix.fork () with
        | 0 ->
            (try
               ignore (Unix.setsid ());
               Unix.dup2 input_fd Unix.stdin;
               Unix.dup2 output_fd Unix.stdout;
               Unix.dup2 error_fd Unix.stderr;
               Unix.close input_fd; Unix.close output_fd; Unix.close error_fd;
               Unix.execv executable [|executable; "run"; p.pc_id|]
             with exn ->
               Printf.eprintf "pc: cannot start solver: %s\n%!" (Printexc.to_string exn);
               Unix._exit 1)
        | pid ->
            let reaped = ref false in
            let stop () =
              kill_group pid;
              if not !reaped then begin
                (* The deadline can precede the child's setsid call. *)
                (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH,_,_) -> ());
                ignore (reap [] pid); reaped := true
              end
            in
            Fun.protect ~finally:stop (fun () ->
              let deadline = Unix.gettimeofday () +. timeout in
              let rec wait () =
                match reap [Unix.WNOHANG] pid with
                | 0, _ ->
                    let left = deadline -. Unix.gettimeofday () in
                    if left <= 0. then (stop (); Timed_out)
                    else begin
                      (try ignore (Unix.select [] [] [] (min 0.01 left))
                       with Unix.Unix_error (Unix.EINTR,_,_) -> ());
                      wait ()
                    end
                | _, status -> reaped := true; Finished status
              in wait ()))))

let judge ~executable ~timeout p fixture =
  let output = Filename.temp_file "pc-output-" ".tmp" in
  Fun.protect ~finally:(fun () -> Sys.remove output) (fun () ->
    let errors = Filename.temp_file "pc-errors-" ".tmp" in
    Fun.protect ~finally:(fun () -> Sys.remove errors) (fun () ->
      let label = p.section ^ "/" ^ fixture.name in
      let failure message =
        Printf.eprintf "FAIL %s: %s\n" label message;
        let stderr = excerpt errors in
        if stderr <> "" then Printf.eprintf "  stderr: %S\n" stderr;
        false
      in
      try
        match execute ~executable ~timeout p fixture output errors with
        | Timed_out -> failure (Printf.sprintf "timeout after %g seconds" timeout)
        | Finished (Unix.WEXITED 0) ->
            (match compare_files fixture.expected output with
             | None -> Printf.eprintf "PASS %s\n" label; true
             | Some d -> failure (Printf.sprintf "first difference at byte %d (0-based)\n  expected %s\n  actual   %s"
                                   d.offset d.expected d.actual))
        | Finished (Unix.WEXITED code) -> failure (Printf.sprintf "exit status %d" code)
        | Finished (Unix.WSIGNALED signal) -> failure (Printf.sprintf "terminated by signal %d" signal)
        | Finished (Unix.WSTOPPED signal) -> failure (Printf.sprintf "stopped by signal %d" signal)
      with Unix.Unix_error (e,fn,arg) -> failure (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message e))
         | Sys_error s -> failure s))

let test ~all ~selected ~specific ~root ~timeout ~executable =
  (* Validate the entire fixture tree before running any solver. *)
  let fixtures = discover all root in
  let passing = ref 0 and failing = ref 0 and unimplemented = ref 0 and fixture_free = ref 0 in
  List.iter (fun p ->
    let cases = Option.value (Hashtbl.find_opt fixtures p.pc_id) ~default:[] in
    if p.solver = None then begin
      incr unimplemented;
      if specific || cases <> [] then Printf.eprintf "UNIMPLEMENTED %s (%s)\n" p.section p.title
    end;
    if cases = [] then begin
      incr fixture_free;
      if specific then Printf.eprintf "NO FIXTURES %s (%s)\n" p.section p.title
    end;
    if p.solver <> None then List.iter (fun case ->
      if judge ~executable ~timeout p case then incr passing else incr failing) cases) selected;
  Printf.eprintf "%d passing · %d failing cases · %d unimplemented · %d fixture-free problems\n"
    !passing !failing !unimplemented !fixture_free;
  if !passing + !failing = 0 then Printf.eprintf "0 cases executed\n";
  if !failing > 0 || (specific && (!unimplemented > 0 || !fixture_free > 0)) then 1 else 0
