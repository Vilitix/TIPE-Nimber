(*Struct.pers.ml*)
(*Tableau persistant*)
module New_Arr = struct
type 'a t = 'a data ref
and 'a data =
|Arr of 'a array
|N of int * 'a * 'a t
|Invalid


let init (n:int) (f:(int -> 'a)):'a data ref = ref (Arr (Array.init n f))


let rec reroot (t:'a t):unit = 
  (*sorte de compression des chemins du union find mais pour un tableau*)
  match !t with
  |Arr _ -> ()
  |N(i, v, t1) -> 
    reroot t1;
    begin 
      match !t1 with
      |Arr a as n ->
        a.(i) <- v;
        t := n;
        t1 := Invalid
      |N(_) |Invalid -> assert false (*après reroot t1 est un Arr*)
    end
    |Invalid -> assert false


let get (t:'a t) (i:int):'a = 
  match !t with
  |Arr a -> a.(i)
  |N _ -> 
    reroot t;
    begin 
      match !t with
      |Arr a -> a.(i)
      |N(_) -> assert false
      |Invalid -> assert false
    end
  |Invalid -> assert false


let set (t:'a data ref) (i:int) (j:'a):'a data ref = 
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
  |Invalid -> assert false
  end


module type PersistentArray = sig
type 'a t
val init : int -> (int -> 'a) -> 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> 'a t
end

(*fin tableau persistant *)
(*Union find*)
module type PersistentUnionFind = sig
  type t
  val create : int -> t
  val find : t -> int -> int
  val union : t -> int -> int -> t
end


 (*foncteur, notion dont on ne s'intéresse pas trop et peut être évitable ici même si plus propre,
  seulement une reprise du document :
  Sylvain Conchon, Jean Christophe Filiâtre : A Persistent Union-Find*)
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
      (*compression des chemins*)

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
      (*union par rang*)
    else h
end
