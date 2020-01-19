(* Chap 1 Exercise 2 *)

module Tree = struct

  type 'a t = Leaf of 'a | Node of 'a t * 'a * 'a t

  module Iterator = struct

    type 'a path =
      | Top
      | Left of 'a path * 'a * 'a t
      | Right of 'a t * 'a * 'a path

    type 'a iterator = Loc of 'a t * 'a path

    exception Fail

    let go_left (Loc (t, p)) =
      match p with
  | Top -> raise Fail
  | Left (father, x, right) -> raise Fail
  | Right (left, x, father) -> Loc (left, Left (father, x, t))

    let go_right (Loc (t, p)) =
      match p with
      | Top -> raise Fail
      | Left (father, x, right) -> Loc (right, Right (t, x, father))
      | Right (left, x, father) -> raise Fail

    let go_up (Loc (t, p)) =
      match p with
  | Top -> raise Fail
  | Left(father, x, right) -> Loc (Node (t, x, right), father)
  | Right(left, x, father) -> Loc (Node (left, x, t), father)

    let go_first (Loc (t, p)) =
      match t with
  | Leaf _ -> raise Fail
  | Node (left, x, right) -> Loc (left, Left (p, x, right))

    let go_second (Loc (t, p)) =
      match t with
  | Leaf _ -> raise Fail
  | Node (left, x, right) -> Loc (right, Right (left, x, p))

    let focus (Loc ((Leaf x | Node (_, x, _)), _)) = x

  end

end

let bfs t =
  let rec aux results = function
    | [] ->
        List.rev results
    | l :: ls ->
        let results = (Tree.Iterator.focus l) :: results in
        try
          aux results (ls @ [ Tree.Iterator.go_first l; Tree.Iterator.go_second l])
        with Tree.Iterator.Fail ->
          aux results ls
  in
  aux [] [Tree.Iterator.Loc (t, Tree.Iterator.Top)]

(* Chap 1 Exercise 3 *)

type e = EInt of int | EMul of e * e | EAdd of e * e

module Exp =
struct
  let int x = EInt x

  let mul a b =
    match a, b with
    | EInt 0, _ | _, EInt 0 -> EInt 0
    | EInt 1, e | e, EInt 1 -> e
    | a, b -> EMul (a, b)

  let add a b =
    match a, b with
    | EInt 0, e | e, EInt 0 -> e
    | a, b -> EAdd (a, b)

  let rec eval = function
    | EInt x -> x
    | EAdd (l, r) -> eval l + eval r
    | EMul (l, r) -> eval l * eval r
end

let example x y z = (* don't change anything to this defintion *)
  Exp.int (Exp.eval (Exp.mul (Exp.int x) (Exp.add (Exp.int y) (Exp.int z))))

(* Chap 2 Exercise 1 *)

module Exp : sig
  type e
  val zero : e
  val int : int -> e
  val mul : e -> e -> e
  val add : e -> e -> e
  val to_string : e -> string
end = struct
  type e =
    | EInt of int
    | EMul of e * e
    | EAdd of e * e

  let zero = EInt 0

  let int x = EInt x

  let mul a b =
    match a, b with
    | EInt 0, _ | _, EInt 0 -> EInt 0
    | EInt 1, e | e, EInt 1 -> e
    | a, b -> EMul (a, b)

  let add a b =
    match a, b with
    | EInt 0, e | e, EInt 0 -> e
    | a, b -> EAdd (a, b)

  let rec to_string = function
    | EInt i -> string_of_int i
    | EMul (l, r) -> "(" ^ to_string l ^ " * " ^ to_string r ^ ")"
    | EAdd (l, r) -> "(" ^ to_string l ^ " + " ^ to_string r ^ ")"
end

(* Chap 2 Exercise 2 *)

module type MultiSet_S = sig

  (* A multi-set of type ['a t] is a collection of values of
     type ['a] that may occur several times. *)
  type 'a t

  (* [occurrences s x] return the number of time [x] occurs
     in [s]. *)
  val occurrences : 'a t -> 'a -> int

  (* The empty set has no element. There is only one unique
     representation of the empty set. *)
  val empty : 'a t

  (* [insert s x] returns a new multi-set that contains all
     elements of [s] and a new occurrence of [x]. Typically,
     [occurrences s x = occurrences (insert s x) x + 1]. *)
  val insert : 'a t -> 'a -> 'a t

  (* [remove s x] returns a new multi-set that contains all elements
     of [s] minus an occurrence of [x] (if [x] actually occurs in
     [s]). Typically, [occurrences s x = occurrences (remove s x) x -
     1] if [occurrences s x > 0]. *)
  val remove : 'a t -> 'a -> 'a t

end

module MultiSet : MultiSet_S = struct
  type 'a t = 'a list

  let occurrences mset s =
    mset
    |> List.filter (fun s' -> s' = s)
    |> List.length

  let empty = []

  let insert mset s =
    s :: mset

  let remove mset s =
    List.filter (fun s' -> s' <> s) mset
end

(* TODO: finish *)
(* module MultiSet : MultiSet_S = struct
 *   type 'a t = ('a * int) list
 * 
 *   let occurrences mset a =
 *     List.assoc a mset
 * 
 *   let empty = []
 * 
 *   let insert mset a =
 *     try
 *       let (_, n) = List.assoc a mset in
 *       List.remove_assoc a mset
 *       |> (fun mset -> (a, n + 1) :: mset)
 *     with
 *       Not_found -> (a, 1) :: mset
 * 
 *   let remove mset a =
 *     List.remove_assoc a mset
 * end *)

let letters word =
  match word with
  | "" -> MultiSet.empty
  | _ ->
    let len = String.length word in
    let rec insert mset index =
      if index >= len
      then mset
      else
        MultiSet.insert mset (String.get word index)
        |> (fun mset -> insert mset (index + 1))
    in
    insert MultiSet.empty 0

let anagram word1 word2 =
  if word1 = word2
  then true
  else if (String.length word1) <> (String.length word2)
  then false
  else
    let len = String.length word1 in
    let letters1 = letters word1 in
    let letters2 = letters word2 in
    let rec compare index =
      if index = len
      then true
      else if
        (MultiSet.occurrences letters1 (String.get word1 index))
        <>
        (MultiSet.occurrences letters2 (String.get word2 index))
      then false
      else compare (index + 1)
    in
    compare 0

(* Chap 3 Exercise 1 *)

module type DictSig = sig
  type ('key, 'value) t
  val empty : ('key, 'value) t
  val add : ('key, 'value) t -> 'key -> 'value -> ('key, 'value) t
  exception NotFound
  val lookup : ('key, 'value) t -> 'key -> 'value
  val remove : ('key, 'value) t -> 'key -> ('key, 'value) t
end

module Dict : DictSig = struct
  type ('key, 'value) t =
    | Empty
    | Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t

  let empty = Empty

  let rec add d k v =
    match d with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (l, k', v', r) ->
        if k = k' then Node (l, k, v, r)
        else if k < k' then Node (add l k v, k', v', r)
        else Node (l, k', v', add r k v)

  exception NotFound

  let rec lookup d k =
    match d with
    | Empty ->
        raise NotFound
    | Node (l, k', v', r) ->
        if k = k' then v'
        else if k < k' then lookup l k
        else lookup r k

  let rec remove d k =
    match d with
    | Empty -> d
    | Node (l, k', v', r) ->
      if k = k'
      then begin
        let rec right_append d_entry d_right =
          match d_entry with
          | Empty -> d_right
          | Node (l, k, v, Empty) -> Node (l, k, v, d_right)
          | Node (l, k, v, r) -> Node (l, k, v, right_append r d_right)
        in
        match (l, r) with
        | (Empty, _) -> r
        | (_, Empty) -> l
        | (Node (l1, k1, v1, r1), n2) -> Node (l1, k1, v1, right_append r1 n2)
      end
      else if k < k'
      then Node (remove l k, k', v', r)
      else Node (l, k', v', remove r k)

end

(* Chap 4 Exercise 1 *)

module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
end

module CharHashedType : Hashtbl.HashedType = struct
  type t = char
  let equal x y =
    x = y
  let hash = Char.code
end

module CharHashtbl = Hashtbl.Make (CharHashedType)

(* module Trie : GenericTrie
 *   with type 'a char_table = 'a CharHashtbl.t =
 * struct
 *   type 'a char_table = 'a CharHashtbl.t
 *   type 'a trie = Trie of 'a option * 'a trie char_table
 * 
 *   let empty () =
 *     Trie (CharHashtbl.create 0)
 * 
 *   let lookup trie w =
 *     "Replace this string with your implementation." ;;
 * 
 *   let insert trie w v =
 *     "Replace this string with your implementation." ;;
 * 
 * end *)
