(**
   This file is part of Discure.

    Author: Victor Nicolet <victorn@cs.toronto.edu>

    Discure is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    DiscuRe is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Discure.  If not, see <http://www.gnu.org/licenses/>.
*)

open Base
module IH = Sets.IH
module IS = Sets.IS
module SM = Sets.SM
module SH = Sets.SH
module IM = Sets.IM
module Config = Config
module Log = Log
module Naming = Naming

let failhere file f s =
  failwith Fmt.(to_to_string (fun frmt () -> pf frmt "[%s][%s]: %s@." file f s) ())

(* Set operations *)
let ( -$ ) is1 is2 = Set.diff is1 is2

let ( +$ ) is1 is2 = Set.union is1 is2

let ( ~$ ) i = Set.singleton (module Int) i

let ( !$ ) s = Set.max_elt s

let ( ?$ ) s = Set.length s

(* Pretty printing *)
let colon = Fmt.(fun f () -> pf f ":@ ")

let semicolon = Fmt.(fun f () -> pf f ";@ ")

let vert f () = Fmt.(if utf_8 f then pf f " 𒑰@;" else pf f "|@")

let arrow f () = Fmt.(if utf_8 f then pf f " →@;" else pf f "->@")

let wrap s = Fmt.(fun f () -> pf f s)

let string_msg msg s = Fmt.(fun f () -> pf f msg s)

let printer_msg msg pp e = Fmt.(fun f () -> pf f msg pp e)

let printer2_msg msg pp e pp2 e2 = Fmt.(fun f () -> pf f msg pp e pp2 e2)

let pp_map pp f m =
  let fl f (_, x) = pp f x in
  Fmt.((box ~indent:2 (braces (list ~sep:semicolon fl))) f (Map.to_alist m))

(**
    Returns a new hashset containing the the keys
    in h1, and values are the pairs of values
    having the same key in h1 and h2 (None for the second
    element if not found).
*)

let ih_join_left (newh : ('a * 'b option) IH.t) (h1 : 'a IH.t) (h2 : 'b IH.t) : unit =
  Hashtbl.iteri
    ~f:(fun ~key:k ~data:v1 ->
      try
        let v2 = Hashtbl.find_exn h2 k in
        Hashtbl.add_exn newh ~key:k ~data:(v1, Some v2)
      with _ -> Hashtbl.add_exn newh ~key:k ~data:(v1, None))
    h1

let identity x = x

let identity2 _ y = y

let is_some = function Some _ -> true | _ -> false

let is_none = function None -> true | _ -> false

(** Convert a varinfo to an expression *)
let ( ==> ) (f : 'a -> 'b) (xo : 'a option) = match xo with Some x -> Some (f x) | None -> None

let ( |> ) (a : 'a) (f : 'a -> 'b) : 'b = f a

let ( @: ) (a : 'a list) (b : int) = List.nth a b

let foi = Float.of_int

let fos = Float.of_string

let ( --> ) (f : 'a -> 'b) (g : 'b -> 'c) x = g (f x)

let ( <=> ) (f : 'a -> 'a) (g : 'a -> 'a) x = f x = g x

let map_2 (f : 'a -> 'b) ((a, b) : 'a * 'a) : 'b * 'b = (f a, f b)

let map_3 (f : 'a -> 'b) ((a, b, c) : 'a * 'a * 'a) : 'b * 'b * 'b = (f a, f b, f c)

let fst (a, _) = a

let snd (_, b) = b

let fst3 (a, _, _) = a

let f2of3 (a, b, _) = (a, b)

let snd3 (_, b, _) = b

let third (_, _, c) = c

(* Mutable conditionals for actions. *)
let _brev b = b := not !b

let _boff b = b := false

let _bon b = b := true

(* If flag true then do nothing, else do and set flag to true. *)
let ( -? ) b f =
  if !b then ()
  else (
    _bon b;
    f )

(* If flag true then do and set flag to false, else do nothing *)
let ( +? ) b f =
  if !b then (
    _boff b;
    f )
  else ()

let bool_of_int64 (i : int64) = Int64.(i = 1L)

let str_contains str sub = ExtLib.String.exists str sub

let str_begins_with sub str = ExtLib.String.starts_with str sub

let str_ends_with sub str = ExtLib.String.ends_with str sub

(** Lists *)
module ListTools = struct
  open List

  let ( -- ) i j =
    if i <= j then
      let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
      aux j []
    else []

  let init (len : int) (f : int -> 'a) : 'a list =
    let rec aux_init l i = if i <= 0 then f 0 :: l else aux_init (f i :: l) (i - 1) in
    aux_init [] len

  let replace_ith (l : 'a list) (k : int) (e : 'a) =
    List.mapi ~f:(fun i e' -> if i = k then e else e') l

  let rec first_nonzero (dc : 'a -> 'a -> int) (l1 : 'a list) (l2 : 'a list) =
    match (l1, l2) with
    | hd1 :: tl1, hd2 :: tl2 ->
        let r = dc hd1 hd2 in
        if r = 0 then first_nonzero dc tl1 tl2 else Some r
    | [], [] -> Some 0
    | _, _ -> None

  let mapoption (f : 'a -> 'b option) (l : 'a list) : 'b list =
    List.map
      ~f:(function Some x -> x | _ -> failwith "x")
      (List.filter ~f:is_some (List.map ~f l))

  (* Generate all pairs of distrinct elements in list *)
  let rec all_pairs lst =
    match lst with [] -> [] | hd :: tl -> List.map ~f:(fun t -> (hd, t)) tl @ all_pairs tl

  (* Generate all k-length subslists of list l *)
  let k_combinations n lst =
    let rec inner acc k lst =
      match k with
      | 0 -> [ [] ]
      | _ -> (
          match lst with
          | [] -> acc
          | x :: xs ->
              let rec accmap acc f = function [] -> acc | x :: xs -> accmap (f x :: acc) f xs in
              let newacc = accmap acc (fun z -> x :: z) (inner [] (k - 1) xs) in
              inner newacc k xs )
    in
    inner [] n lst

  let cartesian3 l1 l2 l3 =
    List.map ~f:(fun x -> List.map ~f:(fun y -> List.map ~f:(fun z -> (x, y, z)) l3) l2) l1

  let non_empty l = not (List.is_empty l)

  let intlist_max li =
    List.fold_left ~f:(fun max elt -> if elt > max then elt else max) ~init:Int.min_value li

  let lmin score_func l =
    let scored_list = List.map ~f:(fun elt -> (score_func elt, elt)) l in
    snd (List.hd_exn (List.sort ~compare:(fun (s1, _) (s2, _) -> Poly.compare s1 s2) scored_list))

  let pair a_li b_li =
    List.fold2 a_li b_li ~init:[] ~f:(fun c_li a_elt b_elt -> c_li @ [ (a_elt, b_elt) ])

  let index l = List.mapi ~f:(fun i t -> (i, t)) l

  let deindex l = List.map ~f:(fun (_, t) -> t) l

  let unpair a_b_li =
    List.fold_left a_b_li ~init:([], []) ~f:(fun (a_li, b_li) (a, b) ->
        (a_li @ [ a ], b_li @ [ b ]))

  let lfst (l : ('a * 'b) list) : 'a list = List.map ~f:(fun (a, _) -> a) l

  let lsnd (l : ('a * 'b) list) : 'b list = List.map ~f:(fun (_, b) -> b) l

  let lthird (l : ('a * 'b * 'c) list) : 'c list = List.map ~f:(fun (_, _, c) -> c) l

  let untriple a_b_c_li =
    List.fold_left a_b_c_li ~init:([], [], []) ~f:(fun (a_li, b_li, c_li) (a, b, c) ->
        (a_li @ [ a ], b_li @ [ b ], c_li @ [ c ]))

  let remove_elt ?(equal = Poly.( = )) e l =
    let rec go l acc =
      match l with
      | [] -> rev acc
      | x :: xs when equal e x -> go xs acc
      | x :: xs -> go xs (x :: acc)
    in
    go l []

  let remove_duplicates l =
    let rec go l acc = match l with [] -> rev acc | x :: xs -> go (remove_elt x xs) (x :: acc) in
    go l []

  let last list = List.nth list (List.length list - 1)

  let rec lexicographic comp lx ly =
    match (lx, ly) with
    | [], [] -> 0
    | hd1 :: tl1, hd2 :: tl2 ->
        let x = comp hd1 hd2 in
        if x = 0 then lexicographic comp tl1 tl2 else x
    | _ :: _, [] -> 1
    | _ -> -1

  (**
     With n the length of l:
     ptake 0 l = []
     take 1 l = hd l
     take (n-1) l = tl l
     take n l = l
  *)
  let take n l = List.take l n

  let prefixes l = List.mapi ~f:(fun i _ -> take (i + 1) l) l

  let remove_last list = match List.rev list with _ :: t -> List.rev t | [] -> []

  let replace_last list elt = match List.rev list with _ :: t -> List.rev (elt :: t) | [] -> []

  let some_hd l = if length l > 0 then Some (hd l) else None

  let some_tl l = if length l > 0 then Some (tl l) else None

  let for_all_i pred l = List.for_all ~f:pred (List.mapi ~f:(fun i e -> (i, e)) l)

  let rassoc ~equal (alist : ('a * 'b) list) (key : 'b) : 'a option =
    try Option.map ~f:fst (List.find ~f:(fun (_, y) -> equal y key) alist) with _ -> None

  let mmax (measure : 'a -> int) (l : 'a list) =
    match l with
    | hd :: tl ->
        let f m e = if measure e > measure m then e else m in
        Some (List.fold ~f ~init:hd tl)
    | _ -> None

  (**
     [is_prefix_sublist pre l] returns [true] if [pre] is a prefix of the list [l].
  *)
  let is_prefix_sublist ?(equal = Poly.equal) (prefix : 'a list) (li : 'a list) =
    let rec fx pre li =
      match (pre, li) with
      | [], [] -> true
      | _, [] -> false
      | [], _ :: _ -> true
      | hd :: tl, hd' :: tl' -> if equal hd hd' then fx tl tl' else false
    in
    if List.is_empty prefix then false else fx prefix li

  (**
     [remove_prefix pre l] removes the prefix [pre] from the list [l]. If [pre] is a prefix of [l] then the returned value is [Some l'] where pre @ l' = l, otherwise the returned value is None.
  *)
  let rec remove_prefix ?(equal = Poly.equal) (prefix : 'a list) (list : 'a list) =
    match (prefix, list) with
    | [], [] -> Some []
    | _, [] -> None
    | [], l -> Some l
    | hd :: tl, hd' :: tl' -> if equal hd hd' then remove_prefix tl tl' else None

  (**
     [remove_one l e] removes one occurrence of the element [e] in [l]. If such an element has been removed, then the functoin return the pair [true, l'] where [l'] is [l] minus one occurence of [e]. Otherwise the function returns [false, []].
  *)
  let remove_one ?(equal = Poly.equal) l e =
    let rec rem l =
      match l with
      | hd :: tl ->
          if equal hd e then (true, tl)
          else
            let b, ntl = rem tl in
            (b, hd :: ntl)
      | [] -> (false, [])
    in
    rem l

  (**
     [remove_intersection l1 l2] removes elements from [l1] and [l2] that are in both lists. It is {b not} equivalent to doing the same operation on thesets of element in l1 and l2. For example, if l1 contains two elements e and l2 only one e, then the first list of the pair returned will still contain an element e.
  *)
  let remove_intersection ?(equal = Poly.equal) l1 l2 : int list * int list =
    let f (nl1, nl2) l1e =
      let removed, nl2' = remove_one ~equal nl2 l1e in
      if removed then (nl1, nl2') else (nl1 @ [ l1e ], nl2')
    in
    List.fold ~f ~init:([], l2) l1
end

module IHTools = struct
  let key_list ih = Hashtbl.fold ~f:(fun ~key:k ~data:_ l -> k :: l) ih ~init:[]

  (**
      Add al the key-value bindings of to_add to add_to only
      if the key is not present in add_to.
  *)
  let add_all add_to bindings_to_add =
    Hashtbl.iteri
      ~f:(fun ~key:k ~data:v ->
        if Hashtbl.mem add_to k then () else Hashtbl.add_exn add_to ~key:k ~data:v)
      bindings_to_add

  let add_list (add_to : 'a IH.t) (getk : 'a -> int) (l : 'a list) =
    List.iter ~f:(fun b -> Hashtbl.add_exn add_to ~key:(getk b) ~data:b) l

  let iter_bottom_up (h : 'a IH.t) (isroot : 'a -> bool) (children : 'a -> 'a list)
      (app : 'a -> unit) : 'a list =
    let select_roots =
      Hashtbl.fold ~init:[] h ~f:(fun ~key:_ ~data:a roots ->
          if isroot a then a :: roots else roots)
    in
    let rec build_botup stack acc =
      match stack with [] -> acc | hd :: tl -> build_botup (tl @ children hd) (hd :: acc)
    in
    let upbots = build_botup select_roots [] in
    List.iter ~f:app upbots;
    upbots
end

let timed f x =
  let t0 = Unix.gettimeofday () in
  let r = f x in
  (r, Unix.gettimeofday () -. t0)

let list_array_map f l = Array.to_list (Array.map ~f (Array.of_list l))

let rec count_map f l ctr =
  match l with
  | [] -> []
  | [ x ] -> [ f x ]
  | [ x; y ] ->
      (* order matters! *)
      let x' = f x in
      let y' = f y in
      [ x'; y' ]
  | [ x; y; z ] ->
      let x' = f x in
      let y' = f y in
      let z' = f z in
      [ x'; y'; z' ]
  | x :: y :: z :: w :: tl ->
      let x' = f x in
      let y' = f y in
      let z' = f z in
      let w' = f w in
      x' :: y' :: z' :: w' :: (if ctr > 500 then list_array_map f tl else count_map f tl (ctr + 1))

let list_map f l = count_map f l 0
