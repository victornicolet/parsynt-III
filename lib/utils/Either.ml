type ('a, 'b) t = Left of 'a | Right of 'b

(** Constructor functions *)
let left a = Left a
let right b = Right b

let either l r = function
  | Left v -> l v
  | Right v -> r v

(** Bifunctor interface *)
let bimap l r = either (fun v -> left (l v)) (fun v -> right (r v))

external id : 'a -> 'a = "%identity"
let const v = fun _ -> v

(** Functor interface *)
let map f = bimap id f
let (<$>) = map
let map_left f = bimap f id

(** Monadic interface *)
let bind m k = either left k m

let return = right
let (>>=) = bind
let throw = left

(** Applicative interface *)
let pure = return
let apply f v = f >>= fun f' -> v >>= fun v' -> pure (f' v')
let (<*>) = apply

(** Turn a function result in a value or an error *)
let try_ f = try pure (f ()) with exn -> throw exn

(** Predicates *)
let is_left v = either (const true) (const false) v
let is_right v = either (const false) (const true) v

let to_string l r = either
    (fun v -> Printf.sprintf "Left (%s)" (l v))
    (fun v -> Printf.sprintf "Right (%s)" (r v))

(** Extract a value of raise an exception *)
let error v = either (fun e -> raise e) id v

(** Silence into an option *)
let hush v = either (const None) (fun v' -> Some v') v

(** Expand from an option *)
let note e = function
  | None -> Left e
  | Some v -> Right v

(** Catamorphism *)
let fold f z = either (const z) (fun v -> f v z)
