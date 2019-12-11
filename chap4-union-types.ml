(* Exercise 1 *)

type index = Index of int

let read a index =
  let Index i = index in
  Array.get a i

let inside a index =
  let Index i = index in
  let len = Array.length a in
  i >= 0 && i < len

let next index =
  let Index i = index in
  Index (i + 1)

let min_index a =
  if Array.length a = 1
  then Index 0
  else
    let rec min_index' a (current_index : index) (min_index : index) =
      let Index current_i = current_index in
      let Index min_i = min_index in
      if current_i = (Array.length a)
      then Index min_i
      else
        let current_v = a.(current_i) in
        let min_v = a.(min_i) in
        if current_v < min_v
        then min_index' a (Index (current_i + 1)) current_index
        else min_index' a (Index (current_i + 1)) min_index
    in
    min_index' a (Index 1) (Index 0)

(* Exercise 2 *)

let find (a : string array) (w : string) : int option =
  let len = Array.length a in
  match len with
  | 0 -> None
  | _ ->
    let rec find' (i : int) =
      if i = len
      then None
      else if w = a.(i)
      then Some i
      else find' (i + 1)
    in
    find' 0

let default_int = function
  | None -> 0
  | Some i -> i

let merge (a : int option) (b : int option) : int option =
  match (a, b) with
  | None, None -> None
  | None, Some y -> Some y
  | Some x, None -> Some x
  | Some x, Some y -> Some (x + y)

(* Exercise 3 *)

type queue = int list * int list

let foo = [1;2;3] in
let bar = [4;5;6] in
foo @ List.rev bar

let is_empty (front, back) =
  let elements = front @ List.rev back in
  List.length elements = 0

let enqueue x (front, back) =
  (front, x :: back)

let split l =
  match l with
  | [] -> ([], [])
  | [el] -> ([], [el])
  | _ ->
    let half = (List.length l) / 2 in
    let rec split_in_half back front =
      if List.length back = half
      then (List.rev front, List.rev back)
      else split_in_half (List.hd front :: back) (List.tl front)
    in
    split_in_half [] l

let dequeue ((front, back) : queue) : int * queue =
  let l = front @ List.rev back in
  (List.hd l, l |> List.tl |> List.rev |> split)

(* Exercise 4 *)

let rec mem x = function
  | [] -> false
  | el :: rest ->
    if el = x
    then true
    else mem x rest

let reverse l =
  let rec reverse' acc = function
  | [] -> acc
  | head :: tail -> reverse' (head :: acc) tail
  in
  reverse' [] l

let append l1 l2 =
  let rec append' l1 l2 =
    match l1 with
    | [] -> l2
    | head :: tail -> append' tail (head :: l2)
  in
  append' (reverse l1) l2

let combine l1 l2 =
  let rec combine' acc l1 l2 =
    match l1, l2 with
    | [], [] -> reverse acc
    | hd1 :: tl1, hd2 :: tl2 ->
      combine' ((hd1, hd2) :: acc) tl1 tl2
  in
  combine' [] l1 l2

let rec assoc l k =
  match l with
  | [] -> None
  | (key, value) :: tail ->
    if key = k
    then Some value
    else assoc tail k

(* Exercice 5 *)

type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3))

let my_example =
  (* 2 * 2 + 3 * 3 *)
  EAdd ((EMul (EInt 2, EInt 2)), (EMul (EInt 3, EInt 3)))

let rec eval = function
  | EInt x -> x
  | EAdd (x, y) -> (eval x) + (eval y)
  | EMul (x, y) -> (eval x) * (eval y)

let factorize e =
  match e with
  | EAdd ((EMul (a , b)),
          (EMul (a', c)))
    when (eval a) = (eval a')
    ->
    EMul (a, (EAdd (b, c)))
  | _ -> e

let expand e =
  match e with
  | EMul (a, (EAdd (b, c)))
    ->
    EAdd ((EMul (a, b)),
          (EMul (a, c)))
  | _ -> e

let simplify e =
  match e with
  | EMul (EInt 0, EInt 0) -> EInt 1
  | EMul (EInt 0, _) -> EInt 0
  | EMul (_, EInt 0) -> EInt 0
  | EMul (EInt 1, e') -> e'
  | EMul (e', EInt 1) -> e'
  | EAdd (e', EInt 0) -> e'
  | EAdd (EInt 0, e') -> e'
  | _ -> e

(* Exercise 6 *)

type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty_trie =
  Trie (None, [])

let example =
  Trie (None,
        [('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
         ('t',
          Trie (None,
                [('e',
                  Trie (None,
                        [('n', Trie (Some 12, []));
                         ('d', Trie (Some 4, []));
                         ('a', Trie (Some 3, []))]));
		             ('o', Trie (Some 7, []))]));
	       ('A', Trie (Some 15, []))])

let rec children_from_char (m : char_to_children) (c : char) : trie option =
  match m with
  | [] -> None
  | (c', t) :: _ when c = c'
    ->
    Some t
  | _ :: tail -> children_from_char tail c

let update_children association_list character trie : char_to_children =
  if children_from_char association_list character = None
  then (character, trie) :: association_list
  else
    let rec update_children' acc rest =
      match rest with
      | [] -> List.rev acc
      | ((character', trie') as pair) :: tail ->
        update_children' ((if character = character' then (character, trie) else pair) :: acc) tail
    in
    update_children' [] association_list

let rec list_car = function
    | "" -> []
    | ch -> (String.get ch 0 ) :: ((String.length ch) - 1
                                   |> (String.sub ch 1)
                                   |> list_car)

let lookup trie w =
  let letters = list_car w in
  let rec lookup' letters trie =
    match (letters, trie) with
    | ([], Trie (Some i, _)) -> Some i
    | (letter :: tail, Trie (_, m)) ->
      match children_from_char m letter with
      | None -> None
      | Some t -> lookup' tail t
  in
  lookup' letters trie

let insert (trie : trie) (word : string) (final_value : int) : trie =
  let letters = list_car word in
  let rec insert' (letters : char list) (trie : trie) : trie =
    match (letters, trie) with
    | ([], Trie (_, trie)) -> Trie (Some final_value, trie)
    | letter :: tail, Trie (value, assoc_list) ->
      match children_from_char assoc_list letter with
      | None -> Trie (value, update_children assoc_list letter (insert' tail empty))
      | Some trie -> Trie (value, update_children assoc_list letter (insert' tail trie))
  in
  insert' letters trie
