


let table = [|[|true;true;true|];[|true;false;true|];[|true;false;false|];[|true;false;false|];[|true;false;false|]|];;
let tab = Projet_Cram.actualiser_union_find table;;

Projet_Cram.print_uf tab table;;
