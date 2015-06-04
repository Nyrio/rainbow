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

  
