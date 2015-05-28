open Printf;;

(* fonction auxiliaire à append *)
let rec append_aux (chan : out_channel) (hashs : (string*string) list) =
  match hashs with
  | [] -> ()
  | x::l -> fprintf chan "%s;%s\n" (fst x) (snd x); append_aux chan l;;

(* fonction qui ajoute les couples de chaînes donnés en argument à la fin du
fichier nommé filename, avec le format "chaine1;chaine2\n" *)
let append (filename : string) (hashs : (string*string) list) =
  let chan = open_out_gen [Open_creat; Open_append] 6 filename
  in append_aux chan hashs;
  close_out chan;;

(* fonction enregistrant une chaîne, c'est-à-dire un couple (last,seed) dans
la liste appropriee de out_array selon le découpage slices *)
let register_chain (slices : string array) (out_array : (string*string) list array) (i : int ref) (imax : int) (seed : string) =
  if !i > imax then
    (i = ref 0;
    for k = 0 to Array.length out_array - 1 do
      append ("data"^(string_of_int k)^".txt") out_array.(k);
      out_array.(k) <- [];
    done)
  else
    let last = Util.full_chain seed
    in let morceau = Decoup.dicho_bornes slices last
       in out_array.(morceau) <- (last,seed)::out_array.(morceau);
          i := !i + 1;;
  
