(* Taken from grenier.baltree
   https://github.com/let-def/grenier/blob/master/baltree/bt1.mli *)

module Tree : sig
  type 'a t = private
    | Leaf
    | Node of int * 'a t * 'a * 'a t
  val leaf : 'a t
  val node : 'a t -> 'a -> 'a t -> 'a t
end = struct
  type 'a t =
    | Leaf
    | Node of int * 'a t * 'a * 'a t

  let size = function
    | Node (s, _, _, _) -> s
    | Leaf -> 0

  let smaller_ell smin smax = (smin < smax) && ((smin land smax) lsl 1 < smax)
  let disbalanced smin smax = smaller_ell smin (smax lsr 1)
  let node_ l x r = Node (size l + 1 + size r, l, x, r)

  let rot_left l x r k = match r with
    | Node (_, rl, y, rr) ->
      k (k l x rl) y rr
    | _ -> assert false

  let rot_right l y r k = match l with
    | Node (_, ll, x, lr) ->
      k ll x (k lr y r)
    | _ -> assert false

  let inc_left l x r k =
    let r = match r with
      | Node (_, rl, y, rr) when smaller_ell (size rr) (size rl) ->
        rot_right rl y rr k
      | _ -> r
    in
    rot_left l x r k

  let inc_right l y r k =
    let l = match l with
      | Node (_, ll, x, lr) when smaller_ell (size ll) (size lr) ->
        rot_left ll x lr k
      | _ -> l
    in
    rot_right l y r k

  let rec node_left l x r =
    if disbalanced (size l) (size r) then
      inc_left l x r node_left
    else
      node_ l x r

  let rec node_right l y r =
    if disbalanced (size r) (size l) then
      inc_right l y r node_right
    else
      node_ l y r

  let leaf = Leaf

  let node l x r = match l, r with
    | Leaf, Leaf -> node_ leaf x leaf
    | l, r when size l < size r ->
      node_left l x r
    | l, r ->
      node_right l x r
end

type 'a interval = {
  lbound: int;
  rbound: int;
  value: 'a;
}

let interval l r value =
  {lbound = Int64.to_int l; rbound = Int64.to_int r; value}

module RMap = struct
  type 'a t = 'a interval Tree.t
  let empty = Tree.leaf

  let singleton interval =
    Tree.node empty interval empty

  let rec add i = function
    | Tree.Leaf -> singleton i
    | Tree.Node (_, l, j, r) ->
      let c = Int.compare i.rbound j.rbound in
      if c < 0
      then Tree.node (add i l) j r
      else Tree.node l j (add i r)

  let rec build_spine bound acc = function
    | Tree.Leaf -> acc
    | Tree.Node (_, l, i, r) ->
      let c = Int.compare i.rbound bound in
      if c >= 0 then
        build_spine bound ((i, r) :: acc) l
      else
        build_spine bound acc r

  let rec expand_spine acc = function
    | Tree.Leaf -> acc
    | Tree.Node (_, l, i, r) ->
      expand_spine ((i, r) :: acc) l

  let _seq_from rmap bound =
    let rec node spine () =
      match spine with
      | [] -> Seq.Nil
      | (i, r) :: spine' ->
        Seq.Cons (i, node (expand_spine spine' r))
    in
    node (build_spine bound [] rmap)

  let list_from rmap bound =
    let rec loop acc = function
      | [] -> acc
      | (i, r) :: spine' -> loop (i :: acc) (expand_spine spine' r)
    in
    loop [] (build_spine bound [] rmap)
end

(*
  Algorithm suggested by Tudor Brindus (@Xyene) and Timothy Li (@FatalEagle)
  Implementation by Frédéric Bour (@let-def)
*)

type 'a t = {
  intervals: 'a interval array;
  maps: 'a RMap.t array;
  mutable last: int;
}

let create count ~f =
  let intervals = Array.init count f in
  Array.fast_sort (fun i1 i2 -> Int.compare i1.lbound i2.lbound) intervals;
  { intervals; maps = Array.make count RMap.empty; last = -1 }

let iter (t : _ t) ~f =
  Array.iter f t.intervals

let closest_key intervals (addr : int) =
  let l = ref 0 in
  let r = ref (Array.length intervals - 1) in
  while !l <= !r do
    let m = !l + (!r - !l) / 2 in
    let lb = intervals.(m).lbound in
    if lb < addr then
      l := m + 1
    else
      r := m - 1
  done;
  if (!l = Array.length intervals) || (intervals.(!l).lbound > addr) then
    decr l;
  assert (!l = -1 || intervals.(!l).lbound <= addr);
  !l

let initialize_until t j =
  let last = t.last in
  if j > last then (
    let cumulative = ref (if last < 0 then RMap.empty else t.maps.(last)) in
    for i = last + 1 to j do
      cumulative := RMap.add t.intervals.(i) !cumulative;
      t.maps.(i) <- !cumulative;
    done;
    t.last <- j;
  )

let query t (addr : int64) =
  let addr = Int64.to_int addr in
  let l = closest_key t.intervals addr in
  if l = -1 then [] else (
    initialize_until t l;
    RMap.list_from t.maps.(l) addr
  )
