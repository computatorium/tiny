(* Synthetic solvers are created only in a disposable project outside this tree. *)
let check b message = if not b then failwith message
let contains text pattern =
  let rec loop i = i + String.length pattern <= String.length text &&
    (String.sub text i (String.length pattern) = pattern || loop (i+1)) in loop 0

let rec mkdir path =
  if not (Sys.file_exists path) then (mkdir (Filename.dirname path); Unix.mkdir path 0o700)

let read path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic (in_channel_length ic))

let write path contents =
  mkdir (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc contents)

let rec copy source target =
  if Sys.is_directory source then begin
    mkdir target;
    Array.iter (fun name -> copy (Filename.concat source name) (Filename.concat target name)) (Sys.readdir source)
  end else write target (read source)

let rec remove path =
  if Sys.is_directory path then begin
    Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
    Unix.rmdir path
  end else Sys.remove path

let command ?(input="/dev/null") cwd program args =
  let output = Filename.temp_file "pc-integration-" ".log" in
  Fun.protect ~finally:(fun () -> Sys.remove output) (fun () ->
    let fd = Unix.openfile output [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
    let pid = Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      flush_all ();
      match Unix.fork () with
      | 0 ->
          (try
             Unix.chdir cwd;
             let ic = Unix.openfile input [Unix.O_RDONLY] 0 in
             Unix.dup2 ic Unix.stdin; Unix.close ic;
             Unix.dup2 fd Unix.stdout; Unix.dup2 fd Unix.stderr; Unix.close fd;
             Unix.execvp program (Array.of_list (program :: args))
           with _ -> Unix._exit 127)
      | pid -> pid) in
    let _,status = Unix.waitpid [] pid in
    let text = read output in
    (match status with Unix.WEXITED code -> code | _ -> -1),text)

let solver = {|
let calls = ref 0
let solve ic oc =
  incr calls;
  let b = Buffer.create 64 in
  (try while true do Buffer.add_char b (input_char ic) done with End_of_file -> ());
  match Buffer.contents b with
  | "raise\n" -> failwith "synthetic exception"
  | "exit\n" -> exit 7
  | "hang\n" -> Printf.eprintf "timeout-pid:%d\n%!" (Unix.getpid ()); while true do () done
  | "signal\n" -> Unix.kill (Unix.getpid ()) Sys.sigterm
  | "count\n" -> Printf.fprintf oc "%d\n" !calls
  | "large\n" -> output_string stderr (String.make 200000 'e'); output_string oc (String.make 200000 'x')
  | "large-fail\n" -> output_string stderr (String.make 200000 'e'); exit 9
  | text -> output_string oc text
|}

let () =
  let source = Unix.realpath Sys.argv.(1) in
  let root = Filename.temp_file "pc-project-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove root) (fun () ->
    List.iter (fun name -> copy (Filename.concat source name) (Filename.concat root name))
      ["dune-project"; "dune"; "catalog"; "lib"; "tools"; "registry"; "bin"];
    copy (Filename.concat source "solutions/dune") (Filename.concat root "solutions/dune");
    for chapter = 1 to 14 do mkdir (Filename.concat root (Printf.sprintf "solutions/ch%02d" chapter)) done;
    mkdir (Filename.concat root "tests");
    let path p = Filename.concat root p in
    mkdir (path "scratch");
    Unix.putenv "TMPDIR" (path "scratch");
    let expect code program args =
      let actual,text = command root program args in
      if actual <> code then failwith (Printf.sprintf "%s %s: expected exit %d, got %d\n%s"
        program (String.concat " " args) code actual text);
      text in
    let build () = ignore (expect 0 "dune" ["build"; "bin/main.exe"]) in
    let cli code args =
      let text = expect code (path "_build/default/bin/main.exe") args in
      check (not (Array.exists (fun name -> String.starts_with ~prefix:"pc-output-" name ||
        String.starts_with ~prefix:"pc-errors-" name) (Sys.readdir (path "scratch")))) "capture file cleanup";
      text in
    build ();
    let listing = cli 0 ["list"] in
    check (contains listing "0 implemented · 112 remaining") "empty registry totals";
    check (List.length (String.split_on_char '\n' listing) = 114) "112 list entries";
    ignore (cli 0 ["help"]); ignore (cli 0 ["--help"]);
    check (contains (cli 0 ["list"; "--chapter"; "14"]) "0 implemented · 8 remaining") "chapter filter";
    List.iter (fun id -> check (contains (cli 1 ["run";id]) "unimplemented") "unimplemented alias")
      ["1.6.1"; "110101"; "pc:110101"; "uva:100"];
    List.iter (fun args -> ignore (cli 2 args))
      [["wat"]; ["run"]; ["run"; "100"]; ["list"; "--chapter"; "0"];
       ["list"; "--chapter"; "15"]; ["test"; "1.6.1"; "--chapter"; "1"];
       ["test"; "--timeout"]; ["test"; "--tests-dir"]; ["test"; "--tests-dir"; "--timeout"; "1"];
       ["test"; "--chapter"; "1"; "--chapter"; "2"]; ["test"; "1.6.1"; "1.6.2"];
       ["test"; "--tests-dir"; "missing"]];
    List.iter (fun seconds -> ignore (cli 2 ["test"; "--timeout"; seconds]))
      ["0"; "-1"; "nan"; "infinity"; "garbage"];
    check (contains (cli 0 ["test"]) "0 cases executed") "empty bulk test";
    ignore (cli 1 ["test"; "1.6.1"]);
    write (path "solutions/ch01/p01.ml") solver;
    build ();
    check (contains (cli 0 ["list"]) "1 implemented · 111 remaining") "automatic add";
    write (path "solutions/ch14/p01.ml") solver;
    build ();
    check (contains (cli 0 ["list"]) "2 implemented · 110 remaining") "qualified chapters allow the same basename";
    Sys.remove (path "solutions/ch14/p01.ml"); build ();
    check (contains (cli 1 ["test"; "uva:100"]) "NO FIXTURES") "specific fixture-free fails";
    let fixture name input expected =
      write (path ("tests/110101/" ^ name ^ ".in")) input;
      write (path ("tests/110101/" ^ name ^ ".out")) expected in
    fixture "01-exact" "exact\n" "exact\n";
    fixture "02-space" "a b\n" "a  b\n";
    fixture "03-newline" "a" "a\n";
    fixture "04-exception" "raise\n" "";
    fixture "05-exit" "exit\n" "";
    fixture "06-timeout" "hang\n" "";
    fixture "07-after" "after\n" "after\n";
    fixture "08-binary" "\000\255\r\n" "\000\255\r\n";
    fixture "09-isolation" "count\n" "1\n";
    fixture "10-isolation" "count\n" "1\n";
    fixture "11-large" "large\n" (String.make 200000 'x');
    fixture "12-signal" "signal\n" "";
    fixture "13-stderr" "large-fail\n" "";
    let result = cli 1 ["test"; "pc:110101"; "--timeout"; "0.5"] in
    List.iter (fun s -> check (contains result s) ("missing test result: " ^ s))
      ["first difference at byte 2"; "first difference at byte 1"; "<EOF>";
       "synthetic exception"; "exit status 7"; "timeout after"; "PASS 1.6.1/07-after";
       "6 passing · 7 failing cases"; "PASS 1.6.1/11-large";
       "terminated by signal"; "[stderr truncated]"];
    check (contains result "\\n") "escaped context";
    let marker = "timeout-pid:" in
    let rec marker_at i = if String.sub result i (String.length marker) = marker then i else marker_at (i+1) in
    let start = marker_at 0 + String.length marker in
    let rec digits i = if i < String.length result && result.[i] >= '0' && result.[i] <= '9' then digits (i+1) else i in
    let pid = int_of_string (String.sub result start (digits start - start)) in
    (match Unix.kill pid 0 with
     | () -> failwith "timed-out child still exists"
     | exception Unix.Unix_error (Unix.ESRCH,_,_) -> ());
    (* Fixture configuration is checked globally, including outside the selection. *)
    write (path "tests/999999/case.in") "";
    check (contains (cli 2 ["test"; "--chapter"; "2"]) "unknown fixture PC ID") "unknown fixtures";
    remove (path "tests/999999");
    write (path "tests/110102/orphan.in") "";
    check (contains (cli 2 ["test"; "110101"]) "unmatched fixture") "orphan input";
    Sys.remove (path "tests/110102/orphan.in");
    write (path "tests/110102/orphan.out") "";
    ignore (cli 2 ["test"]); remove (path "tests/110102");
    write (path "tests/110101/unexpected.txt") ""; ignore (cli 2 ["test"]);
    Sys.remove (path "tests/110101/unexpected.txt");
    remove (path "tests/110101");
    fixture "exact" "hello\n" "hello\n";
    List.iter (fun id ->
      let code,text = command ~input:(path "tests/110101/exact.in") root (path "_build/default/bin/main.exe") ["run";id] in
      check (code = 0 && text = "hello\n") ("implemented run alias " ^ id))
      ["1.6.1"; "110101"; "pc:110101"; "uva:100"];
    ignore (cli 0 ["test"; "uva:100"]);
    ignore (cli 0 ["test"; "--chapter"; "1"]);
    (* Explicit roots work from other working directories. *)
    let code,text = command (Filename.get_temp_dir_name ()) (path "_build/default/bin/main.exe")
      ["test"; "1.6.1"; "--tests-dir"; path "tests"] in
    check (code = 0 && contains text "1 passing") "explicit fixture root";
    ignore (expect 0 "dune" ["runtest"]);
    write (path "tests/110101/exact.out") "changed\n";
    check (contains (expect 1 "dune" ["runtest"]) "FAIL 1.6.1/exact") "fixture edit invalidates runtest";
    remove (path "tests/110101");
    check (contains (expect 0 "dune" ["runtest"]) "0 cases executed") "fixture removal invalidates runtest";
    write (path "tests/110102/added.in") "";
    check (contains (expect 1 "dune" ["runtest"]) "unmatched fixture") "fixture addition invalidates runtest";
    remove (path "tests/110102");
    (* Compile a catalog title with quote and backslash escapes. *)
    let catalog_path = path "catalog/problems.tsv" in
    let catalog = read catalog_path in
    let lines = String.split_on_char '\n' catalog in
    let escaped = "Quoted \"title\" \\ slash" in
    write catalog_path (String.concat "\n" (List.mapi (fun i line -> if i = 1 then
      "1\t1\t1.6.1\t110101\t100\t" ^ escaped else line) lines));
    build (); check (contains (cli 0 ["list"]) escaped) "compiled string escaping";
    write catalog_path catalog;
    Unix.rename (path "solutions/ch01/p01.ml") (path "solutions/ch01/p02.ml");
    build ();
    ignore (cli 1 ["run"; "1.6.1"]);
    check (contains (cli 1 ["test"; "1.6.2"]) "NO FIXTURES") "automatic rename";
    Sys.remove (path "solutions/ch01/p02.ml"); build ();
    check (contains (cli 0 ["list"]) "0 implemented · 112 remaining") "automatic remove";
    List.iter (fun (file,contents,diagnostic) ->
      write (path file) contents;
      check (contains (expect 1 "dune" ["build"; "bin/main.exe"]) diagnostic) ("invalid solution " ^ file);
      Sys.remove (path file))
      ["solutions/ch01/p09.ml",solver,"unknown problem";
       "solutions/p01.ml",solver,"noncanonical solution path";
       "solutions/ch01/p01.ml","let solve _ _ = 42\n","Pc.Harness.solver"];
    build ();
    (* Bootstrapping creates editable files only in this disposable project. *)
    ignore (cli 0 ["init"; "--help"]);
    List.iter (fun args -> ignore (cli 2 ("init" :: args)))
      [[]; ["unknown"]; ["1.6.1"; "extra"]; ["1.6.1"; "--case"; "../escape"];
       ["1.6.1"; "--case"; ""]; ["1.6.1"; "--case"; "sample"; "--case"; "edge"];
       ["1.6.1"; "--input"; "missing"]; ["1.6.1"; "--output"; "missing"];
       ["1.6.1"; "--input"; "missing"; "--output"; "also-missing"];
       ["1.6.1"; "--root"; path "scratch"]];
    check (not (Sys.file_exists (path "solutions/ch01/p01.ml"))) "invalid init must not create a solution";
    check (not (Sys.file_exists (path "tests/110101"))) "invalid init must not create fixtures";
    let result = cli 0 ["init"; "1.6.1"] in
    check (contains result "Created" && contains result "Fill in") "default bootstrap report";
    check (read (path "tests/110101/sample.in") = "" && read (path "tests/110101/sample.out") = "") "empty editable fixtures";
    check (contains (read (path "solutions/ch01/p01.ml")) "Not implemented: 1.6.1") "explicitly unfinished template";
    build ();
    check (contains (cli 0 ["list"]) "1 implemented · 111 remaining") "bootstrap registers on next build";
    check (contains (cli 1 ["test"; "110101"]) "Not implemented: 1.6.1") "empty fixtures must not hide unfinished solve";
    write (path "solutions/ch01/p01.ml") solver;
    fixture "sample" "preserved\n" "preserved\n";
    List.iter (fun id ->
      check (contains (cli 0 ["init"; id]) "Kept") "idempotent bootstrap aliases";
      check (read (path "solutions/ch01/p01.ml") = solver) "preserve edited solution";
      check (read (path "tests/110101/sample.in") = "preserved\n" &&
             read (path "tests/110101/sample.out") = "preserved\n") "preserve edited fixtures")
      ["pc:110101"; "uva:100"; "110101"];
    let binary = "sample\000\255\r\nno-final-newline" in
    write (path "import.in") binary; write (path "import.out") binary;
    let import case = ["init"; "uva:100"; "--case"; case; "--input"; "import.in"; "--output"; "import.out"] in
    ignore (cli 2 (import "sample"));
    check (read (path "tests/110101/sample.in") = "preserved\n") "imports refuse existing fixtures";
    ignore (cli 0 (import "edge-2.binary"));
    check (read (path "tests/110101/edge-2.binary.in") = binary &&
           read (path "tests/110101/edge-2.binary.out") = binary) "byte-exact sample import";
    (* A conflict must be caught before creating the solution or half a pair. *)
    write (path "tests/110201") "existing file";
    ignore (cli 2 ["init"; "2.8.1"]);
    check (not (Sys.file_exists (path "solutions/ch02/p01.ml"))) "preflight directory conflict";
    Sys.remove (path "tests/110201");
    write (path "tests/110102/sample.out") "existing output";
    ignore (cli 2 ["init"; "1.6.2"; "--input"; "import.in"; "--output"; "import.out"]);
    check (not (Sys.file_exists (path "solutions/ch01/p02.ml")) &&
           not (Sys.file_exists (path "tests/110102/sample.in"))) "preflight partial fixture import";
    remove (path "tests/110102");
    write (path "untouched.ml") "preserve me";
    Unix.symlink (path "untouched.ml") (path "solutions/ch01/p02.ml");
    ignore (cli 2 ["init"; "1.6.2"]);
    check (read (path "untouched.ml") = "preserve me") "do not follow destination symlinks";
    Sys.remove (path "solutions/ch01/p02.ml");
    let code,text = command (Filename.get_temp_dir_name ()) (path "_build/default/bin/main.exe")
      ["init"; "1.6.1"; "--root"; root; "--case"; "external";
       "--input"; path "import.in"; "--output"; path "import.out"] in
    check (code = 0 && contains text "Created" && read (path "tests/110101/external.out") = binary) "explicit bootstrap root";
    build ();
    check (contains (cli 0 ["test"; "1.6.1"]) "3 passing") "bootstrapped fixtures are discovered";
    print_endline "temporary-project discovery, CLI, subprocess judging, fixture dependencies, and bootstrap: passed")
