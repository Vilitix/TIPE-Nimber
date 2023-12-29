
module New_Arr = struct

type 'a t = 'a data ref
and 'a data =
|Arr of 'a array
|N of int * 'a * 'a t


let init n f = ref (Arr (Array.init n f))

let rec reroot t = 
  match !t with
  |Arr _ -> ()
  |N(i, v, t1) -> 
    reroot t1;
    begin 
      match !t1 with
      |Arr a as n ->
        let v1 = a.(i) in
        a.(i) <- v;
        t := n;
        t1 := N(i, v1, t)
      |N(_) -> assert false
    end
let get t i = 
  match !t with
  |Arr a -> a.(i)
  |N _ -> 
    reroot t;
    begin 
      match !t with
      |Arr a -> a.(i)
      |N(_) -> assert false
    end

let set t i j = 
  match !t with
  |Arr a as n ->
    begin
      let old = a.(i) in
      a.(i) <-j;
      let res = ref n in
      t := N(i, old, res);
      res
    end
  |N(_) -> assert false

  end


module type PersistentArray = sig
type 'a t
val init : int -> (int -> 'a) -> 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> 'a t
end


module type PersistentUnionFind = sig
  type t
  val create : int -> t
  val find : t -> int -> int
  val union : t -> int -> int -> t
end

module Make(A : PersistentArray) : PersistentUnionFind = struct
  type t = {
    rang: int A.t;
    mutable parent: int A.t
  }

  let create n = {
    rang = A.init n (fun _ -> 0);
    parent = A.init n (fun i -> i)
  }

  let rec find_aux f i =
    let fi = A.get f i in
    if fi = i then
      f, i
    else
      let f, r = find_aux f fi in
      let f = A.set f i r in
      f, r

  let find h x =
    let f, cx = find_aux h.parent x in
    h.parent <- f;
    cx

  let union h x y =
    let cx = find h x in
    let cy = find h y in
    if cx <> cy then 
      begin
        let rx = A.get h.rang cx in
        let ry = A.get h.rang cy in
        if rx > ry then
          { h with parent = A.set h.parent cy cx }
        else if rx < ry then 
          { h with parent = A.set h.parent cx cy }
        else
          { rang = A.set h.rang cx (rx + 1); parent = A.set h.parent cy cx }
      end 
    else h
end
