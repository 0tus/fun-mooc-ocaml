let rec gcd n m =
  let rest = (n mod m) in
  if rest = 0
  then m
  else gcd m rest

let rec multiple_upto n r =
  if r <= 1
  then false
  else if (n mod r) = 0
  then true
  else multiple_upto n (r - 1)

let is_prime n =
  if n = 1
  then true
  else if n = 2
  then true
  else multiple_upto n (n - 1) |> not

let f (int x) = x

let f (x : int) :: int = x

let f x y = if x then (y : int) else y + 1

let f (x : string) : char = String.get x 0

let foo = 1, 2

(* Tuples *)

let exchange k =
  let order_1 = k mod 10 in
  let order_2 = (k - order_1)
                |> (fun x -> x mod 100)
                |> (fun x -> x / 10)
  in
  (order_1 * 10) + order_2

let is_valid_answer (grand_father_age, grand_son_age) =
  (grand_son_age * 4) = grand_father_age
  && (exchange grand_son_age) = (grand_father_age
                                 |> exchange
                                 |> (fun x -> x * 3))

let find answer =
  let (max_grand_father_age, min_grand_son_age) = answer in
  let rec find' max min =
    if max = (min_grand_son_age + 1) && min = min_grand_son_age
    then (-1, -1)
    else if min < min_grand_son_age
    then find' (max - 1) (max - 2)
    else if is_valid_answer (max, min)
    then (max, min)
    else find' max (min -1)
  in
  find' max_grand_father_age min_grand_son_age

(* Records *)

type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

let move p dp =
  let { x; y; z } = p in
  let { dx; dy; dz } = dp in
  { x = x +. dx;
    y = y +. dy;
    z = z +. dz }

(* let foo = { x = 1.; y = 2.; z = 3.; } in
 * let { x; y; z } = foo in
 * z *)

(* let foo = { x = 1.; y = 2.; z = 3.; } in
 * let { x = x'; y = y'; z = z' } = foo in
 * (x', y', z') *)

let next obj =
  let
    { position = pos;
      velocity = dpos }
    = obj
  in
  { position = (move pos dpos);
    velocity = dpos}

let will_collide_soon p1 p2 =
  let radius = 1. in
  let next_p1 = next p1 in
  let next_p2 = next p2 in
  let { position = { x = x1; y = y1; z = z1 } } = next_p1 in
  let { position = { x = x2; y = y2; z = z2 } } = next_p2 in
  (x2 -. x1) ** 2.
  |> (fun acc -> acc +. (y2 -. y1) ** 2.)
  |> (fun acc -> acc +. (z2 -. z1) ** 2.)
  |> sqrt
  |> (fun d -> d < radius *. 2.)


(* let o1 = { position = { x = 1.; y = 2.; z = 3. };
 *            velocity = { dx = 1.; dy = 1.; dz = 1. } }
 * in
 * let o2 = { position = { x = 2.; y = 3.; z = 4. };
 *            velocity = { dx = -1.; dy = -1.; dz = -1. } }
 * in
 * will_collide_soon o1 o2 *)


                will_collide_soon {position = {x = -1.170; y = 1.566; z = -0.581};
                                   velocity = {dx = 0.202; dy = 0.480; dz = 0.106}}
                {position = {x = -1.959; y = 0.680; z = 0.165};
                 velocity = {dx = 0.691; dy = -0.243; dz = 0.088}}

(* Records *)

type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }

let wellformed date =
  let { year; month; day; hour; minute } = date in
  year >= 1
  && month >= 1 && month <= 5
  && day >= 1 && day <= 4
  && hour >= 0 && hour <= 2
  && minute >= 0 && minute <= 1

let next date =
  let { minute } = date in
  let date' = { date with minute = minute + 1 } in
  let rec next' date =
    let { year; month; day; hour; minute } = date in
    if minute > 1
    then next' { date with minute = 0;
                           hour = hour + 1 }
    else if hour > 2
    then next' { date with hour = 0;
                           month = month + 1 }
    else if day > 4
    then next' { date with day = 1;
                           month = month + 1 }
    else if month > 5
    then next' { date with month = 1;
                           year = year + 1 }
    else date
  in
  next' date'

let of_int minutes =
  let hour_in_min = 2 in
  let day_in_min = hour_in_min * 3 in
  let month_in_min = day_in_min * 4 in
  let year_in_min = month_in_min * 5 in
  let date = { year = 0; month = 0; day = 0; hour = 0; minute = 0 } in
  (date, minutes)
  |> (fun (d, m) -> ({ d with year = (m / year_in_min) + 1 }, m mod year_in_min))
  |> (fun (d, m) -> ({ d with month = (m / month_in_min) + 1 }, m mod month_in_min))
  |> (fun (d, m) -> ({ d with day = (m / day_in_min) + 1 }, m mod day_in_min))
  |> (fun (d, m) -> ({ d with hour = m / hour_in_min }, m mod hour_in_min))
  |> (fun (d, m) -> { d with minute = m })

of_int 6

(* Arrays *)

let min' a =
  let length = Array.length a in
  let rec min a i min_i min_v =
    if i = length
    then (min_i, min_v)
    else
      let value = a.(i) in
      if value < min_v
      then min a (i + 1) i value
      else min a (i + 1) min_i min_v
  in
  match length with
  | 0 -> (0, 0)
  | 1 -> (0, a.(0))
  | _ -> min a 1 0 a.(0)


let min a =
  let (_, index) = min' a in
  index

let min_index a =
  let (index, _) = min' a in
  index

let it_scales =
  "yes"

let is_sorted a =
  let length = Array.length a in
  let rec is_sorted' a i =
    if i = length
    then true
    else
      let s1 = a.(i - 1) in
      let s2 = a.(i) in
      if String.compare s1 s2 = -1
      then is_sorted' a (i + 1)
      else false
  in
  match length with
  | 0 -> true
  | 1 -> true
  | _ -> is_sorted' a 1

let find dict word =
  let length = Array.length dict in
  match length with
  | 0 -> -1
  | 1 -> if word = dict.(0) then 0 else -1
  | _ ->
    let rec find' i len =
      let mid = len / 2 in
      match mid with
      | 0 -> if word = dict.(i) then i else -1
      | _ ->
        let i' = i + mid in
        let comp = String.compare word dict.(i') in
        if comp = 0
        then i'
        else if comp < 0
        then find' i mid
        else find' i' (len - mid)
    in
    find' 0 length
