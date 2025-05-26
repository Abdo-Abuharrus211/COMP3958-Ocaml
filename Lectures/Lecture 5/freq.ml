module M = Map.Make(String)

let inc word map =
  M.update word (function | None -> Some 1 | Some n -> Some (n + 1)) map

let count () =
  let rec aux map =
    try
      aux (Scanf.scanf " %s" (fun x ->
          if x = "" then raise End_of_file
          else inc x map))
    with
    | _ -> map
  in M.bindings @@ aux M.empty

