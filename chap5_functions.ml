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

let rec equal_on_common =
  function l1 ->
  function l2 ->
  | [],_ -> true
  | _,[] -> true
  | h1::r1,h2::r2 -> h1=h2 && equal_on_common r1 r2
