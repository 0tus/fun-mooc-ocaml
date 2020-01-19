(* Exercise 1 *)

type report = message list
and message = string * status
and status = Successful | Failed

type 'a result = Ok of 'a
               | Error of exn

let exec f x =
  try Ok (f x)
  with
    e -> Error e

let exn_to_string (exn : exn) : string =
  "foo"

(* exn_to_string exists in the exercise's scope *)
let compare user reference to_string =
  match user with
  | Ok value ->
    if user = reference
    then (String.concat " " ["got correct value"; (to_string value)], Successful)
    else (String.concat " " ["got unexpected value"; (to_string value)], Failed)
  | Error exn ->
    if user = reference
    then (String.concat " " ["got correct exception"; (exn_to_string exn)], Successful)
    else (String.concat " " ["got unexpected exception"; (exn_to_string exn)], Failed)

let compare user reference to_string =
  match user with
  | Ok value when user = reference ->
    (String.concat " " ["got correct value"; (to_string value)], Successful)
  | Ok value ->
    (String.concat " " ["got unexpected value"; (to_string value)], Failed)
  | Error exn when user = reference ->
    (String.concat " " ["got correct exception"; (exn_to_string exn)], Successful)
  | Error exn ->
    (String.concat " " ["got unexpected exception"; (exn_to_string exn)], Failed)

let test user reference sample to_string : report =
  let final_length = 10 in
  let rec test' acc =
    if List.length acc = final_length
    then List.rev acc
    else
      let sample_value = sample () in
      let user_result = exec user sample_value in
      let reference_result = exec reference sample_value in
      let message = compare user_result reference_result to_string in
      test' (message :: acc)
  in
  test' []

(* Exercise 2 *)

let print_int_list l =
  List.map string_of_int l
  |> String.concat "\n"
  |> print_string

let print_every_other k l =
  let rec print_every_other' index rest =
    match (index, rest) with
    | (_, []) -> ()
    | (0, head :: tail) | (_, head :: tail) when index mod k = 0 ->
      print_int head;
      print_every_other' (index + 1) tail
    | (_, _ :: tail) -> print_every_other' (index + 1) tail
  in
  print_every_other' 0 l

let rec print_list print l =
  match l with
  | [] -> ()
  | head :: tail ->
    print head;
    print_string "\n";
    print_list print tail

(* Exercise 3 *)

type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list

let list_init n fn =
  match n with
  | 0 -> []
  | _ ->
    let rec list_init' acc i =
      if i = n
      then List.rev acc
      else list_init' (fn i :: acc) (i + 1)
    in
    list_init' [] 0

let print_path path =
  path
  |> String.concat "/"
  |> print_string

let print_pipes lvl content =
  let pipes = list_init lvl (fun _ -> "|") in
  content
  |> List.rev_append pipes
  |> String.concat " "
  |> print_string

let print_file lvl name =
  [name]
  |> print_pipes lvl

let print_symlink lvl name path =
  [name; "->"; String.concat "/" path]
  |> print_pipes lvl

let print_broken_symlink lvl name =
  [name; "->"; "INVALID"]
  |> print_pipes lvl

let print_dir lvl name =
  ["/" ^ name]
  |> print_pipes lvl

let print_filesystem root =
  let rec print_filesystem' lvl items =
    match items with
    | [] -> ()
    | (name, node) :: tail ->
      let _ =
        match node with
        | File -> print_file lvl name;
          print_string "\n"
        | Dir filesystem ->
          print_dir lvl name;
          print_string "\n";
          print_filesystem' (lvl + 1) filesystem
        | Symlink path ->
          print_symlink lvl name path;
          print_string "\n"
      in
      print_filesystem' lvl tail
  in
  print_filesystem' 0 root

let resolve sym path =
  let rec resolve' acc path =
    match path with
    | [] -> List.rev acc
    | head :: tail when head = ".." ->
      begin match acc with
      | [] -> resolve' [] tail (* This is incorrect but seems to work as indicated in the comments *)
      | _ -> resolve' (List.tl acc) tail
      end
    | head :: tail ->
      resolve' (head :: acc) tail
  in
  resolve' (sym |> List.rev |> List.tl) path

let file_exists (root : filesystem) (source_path : string list) : bool =
  let rec file_exists' fs path =
    match (fs, path) with
    | _, [] -> false
    | [], _ -> false
    | fs, name :: [] ->
      (try
         let node = List.assoc name fs in
         match node with
         | File -> true
         | Symlink sym_path ->
           (try
              let real_path = resolve source_path sym_path in
              file_exists' root real_path
            with
              _ -> false)
         | _ -> false
       with
         Not_found -> false)
    | fs, name :: tail ->
      (try
         let node = List.assoc name fs in
         match node with
         | Dir fs' -> file_exists' fs' tail
         | _ -> false
       with
         Not_found -> false)
  in
  file_exists' root source_path

(* v2 that handles broken symlinks *)
let print_filesystem root =
  let rec print_filesystem' current_path fs =
    match fs with
    | [] -> ()
    | (name, node) :: tail ->
      let _ =
        match node with
        | File -> print_file (List.length current_path) name;
          print_string "\n"
        | Dir filesystem ->
          print_dir (List.length current_path) name;
          print_string "\n";
          print_filesystem' (current_path @ [name]) filesystem
        | Symlink sym_path ->
          if begin
            let abs_path = current_path @ [name] in
            let new_path = resolve sym_path abs_path in
            file_exists root new_path end
          then begin
            print_symlink (List.length current_path) name sym_path;
            print_string "\n" end
          else begin
            print_broken_symlink (List.length current_path) name;
            print_string "\n" end
      in
      print_filesystem' current_path tail
  in
  print_filesystem' [] root


(* Sequences and iterations Exercise 1 *)

let is_multiple i x =
  i mod x = 0

let output_multiples x n m =
  for i=n to m do
    if is_multiple i x
    then begin
      print_int i;
      print_char ',' end
    else ()
  done

exception Stop_it

let display_sign_until_zero f m =
  try
    for i = 0 to m do
      let value = f i in
      if value > 0 then print_string "positive\n"
      else if value < 0 then print_string "negative\n"
      else if value = 0 then (print_string "zero\n"; raise Stop_it)
    done
  with
    Stop_it -> ()

(* Sequences and iterations Exercise 2 *)

type image = int -> int -> bool

let all_white = fun x y -> false

let all_black = fun x y -> true

let checkers = fun x y -> y/2 mod 2 = x/2 mod 2

let square cx cy s = fun x y ->
  let minx = cx - s / 2 in
  let maxx = cx + s / 2 in
  let miny = cy - s / 2 in
  let maxy = cy + s / 2 in
  x >= minx && x <= maxx && y >= miny && y <= maxy

let disk cx cy r = fun x y ->
  let x' = x - cx in
  let y' = y - cy in
  (x' * x' + y' * y') <= r * r

type blend =
  | Image of image
  | And of blend * blend
  | Or of blend * blend
  | Rem of blend * blend

let display_image width height f_image =
  for line = 0 to height do
    for column = 0 to width do
      let x = column in
      let y = line in
      print_string begin
        if f_image x y
        then "#"
        else " "
      end
    done;
    print_string "\n"
  done

let _ =
  display_image 10 10 (disk 5 5 5)

let rec render blend x y =
  match blend with
  | Image f_image -> f_image x y
  | And (b, b') -> (render b x y) && (render b' x y)
  | Or (b, b') -> (render b x y) || (render b' x y)
  | Rem (b, b') -> (render b x y) && (render b' x y) |> not

let display_blend width height blend =
  let f_image = render blend in
  display_image width height f_image

(* Mutable Arrays Exercise 1 *)

(* let rotate a =
 *   let n = Array.length a in 
 *   let v = a.(0) in
 *   for i = 0 to n-2 do
 *     a.(i) <- a.(i+1)
 *   done;
 *   a.(n-1)<-v ;; *)

let rotate a =
  match a with
  | [||] -> ()
  | _ ->
    let n = Array.length a in 
    let v = a.(0) in
    for i = 0 to n - 1 do
      a.(i) <- begin
        if i = n - 1
        then v
        else a.(i + 1)
      end
    done

let rotate_by a n =
  match (a, n) with
  | [||], _ -> ()
  | _, 0 -> ()
  | _ ->
    let al = Array.to_list a in
    let len = Array.length a in
    let n' = if n < 0 then len + n else n in
    for i = 0 to len - 1 do
      let i' = (i + n') mod len in
      a.(i) <- List.nth al i'
    done

(* Mutable Arrays Exercise 2 *)

type stack = int array
exception Full
exception Empty

let create size =
  Array.make (size + 1) 0

let push buf elt =
  let size = buf.(0) in
  if size = Array.length buf - 1
  then raise Full
  else begin
    buf.(0) <- size + 1;
    buf.(size + 1) <- elt
  end

let append buf arr =
  let rec append' elts =
    match elts with
    | [] -> ()
    | head :: tail -> begin
        push buf head;
        append' tail
      end
  in
  append' (arr |> Array.to_list |> List.rev)

let pop buf =
  if buf.(0) = 0
  then raise Empty
  else
    let value = buf.(0) |> fun i -> buf.(i) in
    buf.(0) <- buf.(0) - 1;
    value

(* Mutable record fields *)

type 'a xlist =
  { mutable pointer : 'a cell }
and 'a cell =
  | Nil
  | List of 'a * 'a xlist

let nil () =
  { pointer = Nil }

let cons elt rest =
  { pointer = List (elt, rest) }

exception Empty_xlist

let head l =
  match l.pointer with
  | Nil -> raise Empty_xlist
  | List (x, _) -> x

let tail l =
  match l.pointer with
  | Nil -> raise Empty_xlist
  | List (_, tail) -> tail

let add a l =
  let value = l.pointer in
  l.pointer <- List (a, {pointer = value})

let chop l =
  match l.pointer with
  | Nil -> raise Empty_xlist
  | List (head, tail) ->
    l.pointer <- tail.pointer

let rec append l l' =
  match l.pointer with
  | Nil ->
    l.pointer <- l'.pointer
  | List (_, tail) ->
    append tail l'

let rec filter p l =
  match l.pointer with
  | Nil -> ()
  | List (head, tail) ->
    if p head
    then filter p tail
    else begin
      l.pointer <- tail.pointer;
      filter p l
    end

(* References Exercise 1 *)

exception Empty

let swap ra rb =
  let va = !ra in
  let vb = !rb in
  ra := vb;
  rb := va

let update r f =
  let v = !r in
  r := f v;
  v

let move l1 l2 =
  match !l1 with
  | [] -> raise Empty
  | head :: tail ->
    begin
      l1 := tail;
      l2 := head :: !l2
    end

let reverse l =
  let l_in = ref l in
  let l_out = ref [] in
  begin
    try
      while true do
        move l_in l_out
      done;
    with
      _ -> ()
  end;
  !l_out

(* References Exercise 2 *)

let read_lines () =
  let sl = ref [] in
  let rec aux () =
    try
      sl := read_line () :: !sl ;
      aux ()
    with
      End_of_file -> List.rev !sl
  in
  aux ()
