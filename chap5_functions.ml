(* Exercise 1 *)

let rec last_element = function
  | [] -> invalid_arg "last_element"
  | [el] -> el
  | _ :: tail -> last_element tail

let rec is_sorted = function
  | ([] | [_]) -> true
  | (x :: (y :: tail as rest)) when x < y -> is_sorted rest
  | _ -> false

(* Exercise 2 *)

type int_ff = int -> int

let compose list =
  match List.rev list with
  | [] -> (function x -> x)
  | f_init :: rest ->
    let rec compose' f1 = function
      | [] -> f1
      | f2 :: tail -> compose' (function x -> f2 (f1 x)) tail
    in
    compose' f_init rest

let rec fixedpoint f v1 delta =
  let v2 = (f v1) in
  let local_delta = v1 -. v2 in
  let local_delta = if local_delta > 0. then local_delta else local_delta *. -1. in
  if local_delta < delta
  then v1
  else fixedpoint f v2 delta

(* Exercise 3 *)
(* TODO: ask why the compiler cannot detect that l1 and l2 are fully pattern matched *)

let rec equal_on_common =
  function l1 ->
  function l2 ->
    if l1 = [] then true
    else if l2 = [] then true
    else
      let h1::r1 = l1 in
      let h2::r2 = l2 in
      h1=h2 && equal_on_common r1 r2

(* Exercise 4 *)

let ccr =
  let angle_operation x = x /. 2. |> cos in
  fun a ->
    let a' = angle_operation a in
    fun b ->
      let b' = angle_operation b *. a' in
      fun c ->
        let c' = angle_operation c *. b' *. 8. in
        fun s ->
          s /. c'

(* Exrecise 5 *)

type operation =
  | Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list

let rec lookup_function name env =
  match env with
  | [] -> invalid_arg "lookup_function"
  | (name', fn) :: _ when name = name' -> fn
  | _ :: tail -> lookup_function name tail

let add_function (name : string) (op : int -> int -> int) (env : env) : env =
  let rec add_function' old_env new_env =
    match old_env with
    | [] -> (name, op) :: new_env |> List.rev
    | (name', _) :: tail when name = name' ->
      List.rev_append new_env ((name', op) :: tail)
    | head :: tail -> add_function' tail (head :: new_env)
  in
  add_function' env []

let my_env =
  [("add", (+));
   ("sub", ( * ));
   ("mul", (-));
   ("div", (/));
   ("min", fun x y -> if x < y then x else y)]

let rec compute (env : env) (op : operation) : int =
  match op with
  | Value x -> x
  | Op (name, op1, op2) ->
    let f = lookup_function name env in
    match (op1, op2) with
    | Value x, Value y ->
      f x y
    | Op _, Value y ->
      f (compute env op1) y
    | Value x, Op _ ->
      f x (compute env op2)
    | Op _, Op _ ->
      f (compute env op1) (compute env op2)

(* I feel like cheating here but it still works for the exercise *)
let rec compute_eff env = function
  | Value x -> x
  | Op (name, op1, op2) ->
    match (op1, op2) with
    | Value x, Value y ->
      (lookup_function name env) x y
    | Op _, Value y ->
      (lookup_function name env) (compute_eff env op1) y
    | Value x, Op _ ->
      (lookup_function name env) x (compute_eff env op2)
    | Op _, Op _ ->
      (lookup_function name env) (compute_eff env op1) (compute_eff env op2)

(* Exercise 6 *)

type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;

let wrap l =
  let rec wrap' acc l =
    match l with
    | [] -> List.rev acc
    | head :: tail -> wrap' ([head] :: acc) tail
  in
  wrap' [] l

let rec tree_map f tree =
  match tree with
  | Leaf x -> Leaf (f x)
  | Node (left_tree, x, right_tree) ->
    Node (tree_map f left_tree, f x, tree_map f right_tree)

(* Exercise 7 *)

let filter p l =
  let rec filter' acc rest =
    match rest with
    | [] -> List.rev acc
    | head :: tail ->
      filter'
        (if p head
         then head :: acc
         else acc)
        tail
  in
  filter' [] l

let partition p l =
  List.fold_right
    (fun elem ((lpos, rpos) : 'a list * 'a list) ->
       if p elem
       then (elem :: lpos, rpos)
       else (lpos, elem :: rpos))
    l
    ([], [])

let rec sort l =
  match l with
  | [] -> []
  | head :: tail ->
    let (lpos, rpos) = partition (fun x -> x < head) tail in
    List.append (sort lpos) (head :: (sort rpos))


(* List.fold_right (fun x acc -> Printf.printf "%i\n" x; x + acc) [1;2;3;4] 0 *)
