(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)

let build_word index len x word =
  string_init len (fun i -> if i = index then chr(ord('a') + x) else string_get_at word i)

let build_list len word work=
  for i = 0 to (len -1) do
    for x = 0 to 25 do
      work(build_word i len x word)
    done
  done

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let
list_of_buddies(word: string): string list = 
let len = string_length word in
  filter (fun x -> x <> word) (list_make_fwork (build_list len word))

