let () =
  try
    match Array.to_list Sys.argv with
    | _ :: catalog :: root :: sources ->
        let problems = Discovery.Catalog.load catalog in
        print_string (Discovery.Catalog.generate ~root problems sources)
    | _ -> prerr_endline "usage: generate CATALOG SOLUTIONS_ROOT [SOURCE ...]"; exit 2
  with
  | Discovery.Catalog.Invalid s | Sys_error s -> prerr_endline ("catalog: " ^ s); exit 2
