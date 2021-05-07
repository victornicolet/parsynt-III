open Base
module S = Str

(** Set modules **)
module IntHash = struct
  type t = int

  let equal i j = i = j

  let hash i = i land Int.max_value
end

module IH = Hashtbl.M (Int)

module SM = struct
  include Map.M (String)

  let empty : 'a t = Map.empty (module String)

  let from_bindings (bindings : (string * 'a) list) : 'a t =
    Map.of_alist_reduce ~f:(fun x _ -> x) (module String) bindings

  let find (key : string) (m : 'a t) : 'a option = Map.find m key
end

module SH = struct
  include Hashtbl.M (String)

  let create (size : int) : 'v t = Hashtbl.create ~size (module String)

  let add (table : 'v t) (key : string) (data : 'v) : unit =
    match Hashtbl.add table ~key ~data with _ -> ()

  let set (table : 'v t) (key : string) (data : 'v) : unit = Hashtbl.set table ~key ~data

  let clear (table : 'v t) = Hashtbl.clear table

  let mem (table : 'v t) (s : string) = Hashtbl.mem table s

  let fold (f : string -> 'v -> 'a -> 'a) (table : 'v t) (init : 'a) : 'a =
    Hashtbl.fold table ~init ~f:(fun ~key ~data accum -> f key data accum)
end

module IS = struct
  include Set.M (Int)

  let empty = Set.empty (module Int)
end

module IntegerMap = Map.M (Int)

module IM = struct
  include IntegerMap

  let empty : 'v t = Map.empty (module Int)

  let keyset (imt : 'v t) : IS.t =
    Set.of_list (module Int) (List.map ~f:(fun (a, _) -> a) (Map.to_alist imt))

  let add_all (add_to : 'v t) (to_add : 'v t) : 'v t =
    Map.fold ~init:add_to to_add ~f:(fun ~key ~data mp ->
        if Map.mem add_to key then mp else Map.add_exn ~key ~data mp)

  let update_all (add_to : 'v t) (to_add : 'v t) : 'v t =
    Map.merge add_to to_add ~f:(fun ~key:_ valu ->
        match valu with `Both (_, vr) -> Some vr | `Left v -> Some v | `Right v -> Some v)

  let join_opt (map_a : 'v t) (map_b : 'v t) : 'v t =
    let f = Option.is_some in
    let a = Map.filter map_a ~f and b = Map.filter map_b ~f in
    add_all a b

  let inter (a : 'v t) (b : 'v t) : 'v t =
    let f ~key:_ data = match data with `Both (v1, _) -> Some v1 | `Left _ | `Right _ -> None in
    Map.merge ~f a b

  let is_disjoint ?(non_empty = fun _ _ -> true) (a : 'v t) (b : 'v t) : bool =
    try
      Map.fold ~init:true
        ~f:(fun ~key ~data bol ->
          if non_empty key data then if Map.mem a key then failwith "iom" else bol else bol)
        b
    with Failure _ -> false

  let disjoint_sets (im1 : 'v t) (im2 : 'v t) : 'v t * 'v t * 'v t * 'v t =
    let im1_in_im2 =
      Map.fold
        ~init:(Map.empty (module Int))
        ~f:(fun ~key ~data mmap -> if Map.mem im2 key then Map.add_exn ~key ~data mmap else mmap)
        im1
    in
    let im2_in_im1 =
      Map.fold ~init:im1
        ~f:(fun ~key ~data map -> if Map.mem im1 key then Map.add_exn ~key ~data map else map)
        im2
    in
    let im1_only =
      Map.fold
        ~init:(Map.empty (module Int))
        ~f:(fun ~key ~data map -> if Map.mem im2 key then map else Map.add_exn ~key ~data map)
        im1
    in
    let im2_only =
      Map.fold
        ~init:(Map.empty (module Int))
        ~f:(fun ~key ~data map -> if Map.mem im1 key then map else Map.add_exn ~key ~data map)
        im2
    in
    (im1_in_im2, im2_in_im1, im1_only, im2_only)

  let of_ih (ih : 'v IH.t) : 'v t =
    Hashtbl.fold ih ~init:empty ~f:(fun ~key ~data m -> Map.add_exn ~key ~data m)

  let to_alist (im : 'v t) : (int * 'v) list = Map.to_alist ~key_order:`Decreasing im

  let of_alist (im : (int * 'v) list) : [ `Duplicate_key of int | `Ok of 'v t ] =
    Map.of_alist (module Int) im
end
