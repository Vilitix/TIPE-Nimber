  type union_find = {
    parent: int array;
    rang: int array;
    taille_classe : (int*int*int*int) array;
  }


  let init n =
    { parent = Array.init n (fun i -> i);
      rang = Array.make n 0; 
      taille_classe = Array.init n (fun i -> (i,i,i,i));
    }
   
  let rec find uf x =
    if uf.parent.(x) = x then x
    else begin
      uf.parent.(x) <- find uf uf.parent.(x);
      uf.parent.(x)
    end
   
  let union uf x y =
    let racine_x = find uf x in
    let racine_y = find uf y in
    if racine_x <> racine_y then begin
      if uf.rang.(racine_x) < uf.rang.(racine_y) then
        begin
        uf.parent.(racine_x) <- racine_y;
        end
      else if uf.rang.(racine_x) > uf.rang.(racine_y) then
        begin
        uf.parent.(racine_y) <- racine_x;
        end
      else 
        begin
        uf.parent.(racine_y) <- racine_x;
        uf.rang.(racine_x) <- uf.rang.(racine_x) + 1
        end
    end

let get_tab uf = 
  uf.parent

let get_tab_c uf = 
  uf.taille_classe