
exception Bad_float

let check_float x =
  let c = classify_float x in
  if (c = FP_infinite) || (c = FP_nan) then raise Bad_float;
  x

let check_float_fun f x = check_float (f x)

let empty_or_comment line =
  (Pcre.pmatch ~pat:"^\\s*$" line) ||
  (Pcre.pmatch ~pat:"^\\s*#" line)

let not_empty_or_comment line = not (empty_or_comment line)

let chomp str =
  Pcre.replace ~pat:"\\s$" ~templ:"" str

let remove_space str =
  Pcre.replace ~pat:"\\s" ~templ:"" str

let file_to_line_list fname =
  let lines = ref [] in
  let cin = open_in fname in
  try
    while true do
      let line = chomp (input_line cin) in
      lines := line::!lines;
    done;
    close_in cin;
    List.rev !lines
  with
      End_of_file ->
        close_in cin;
        List.rev !lines (* adding with :: reverses order *)


(* read in a channel, and convert every line with of_line that
   satisfies filter *)
let channel_to_thing_list cin filter of_line =
  let things = ref [] in
  try
    while true do
      let line = chomp (input_line cin) in
      if filter line then
        things := (of_line line)::!things;
    done;
    List.rev !things
  with
      End_of_file ->
        List.rev !things (* adding with :: reverses order *)

(* read in a file, and convert every line with of_line that satisfies
   filter *)
let file_to_thing_list fname filter of_line =
  let ch = open_in fname in
  let l = channel_to_thing_list ch filter of_line in
  close_in ch;
  l




(* parses a ctl file and then makes a series of functions which access
   the parameters specified. the existence_fun is a bool
   function which states if a certain variable which has been
   defined or no. subsequent definitions mask previous ones. *)

let ctl_to_vars fname_list =
  let ih = Hashtbl.create 5 in (* integers *)
  let fh = Hashtbl.create 5 in (* floats *)
  let bh = Hashtbl.create 5 in (* bools *)
  let sh = Hashtbl.create 5 in (* strings *)
  let eh = Hashtbl.create 5 in (* existence - if defined or not *)
  List.iter (
    fun fname ->
      let cin = open_in fname in
      let rec readlines acc =
        try readlines ((input_line cin)::acc)
        with End_of_file -> close_in cin; List.rev acc
      in
      let no_space = Pcre.replace ~pat:"\\s" ~templ:"" in
      List.iter (
        fun str ->
          if not (empty_or_comment str) then (
            let info = Pcre.split ~pat:"\\s:\\s" str in
            let name = List.nth info 0
            and var_type = List.nth info 1
            and v_str = List.nth info 2
            in

            let clean_type = no_space var_type in
            let clean_var = no_space v_str in
            if clean_type = "i" then
              Hashtbl.add ih (no_space name) (int_of_string clean_var)
            else if clean_type = "f" then
              Hashtbl.add fh (no_space name) (float_of_string clean_var)
            else if clean_type = "b" then
              Hashtbl.add bh (no_space name) (bool_of_string clean_var)
            else if clean_type = "s" then
              Hashtbl.add sh (no_space name) clean_var
            else
              failwith "unknown variable type\n";
            Hashtbl.add eh (no_space name) true;
          )
      ) (readlines []);
  ) fname_list;
  let make_fun h =
    fun str ->
      try Hashtbl.find h str with
      Not_found -> failwith (str^" not found in ctl file!\n")
  in
  let existence_fun str =
    try Hashtbl.find eh str with
    Not_found -> false
  in
  (make_fun ih, make_fun fh, make_fun bh, make_fun sh, existence_fun)



let look_for_file possib_list =
  let choice =
    List.fold_left (
      fun c p ->
        match c with
            Some name -> Some name
          | None -> if Sys.file_exists p then Some p else None
    ) None possib_list
  in
  match choice with
      Some name -> name
    | None -> failwith
        ("none of "^(String.concat " or " possib_list)^" exist!")


let list_remove l_orig rem_i =
  assert (rem_i >= 0 && rem_i < (List.length l_orig));
  let the_x = ref (List.hd l_orig) in
  let rec aux l i =
  match l with
      x::lp ->
        if i <> rem_i then
          x::(aux lp (i+1))
        else (
          the_x := x;
          aux lp (i+1) (* skip x *)
        )
    | [] -> []
  in
  (!the_x, aux l_orig 0)

