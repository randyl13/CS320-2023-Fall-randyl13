#use "./../../assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;


let the_ln2_stream: float stream =
  let rec helper n i sum: float stream = fun () ->
    if n = 1 then
      StrCons (1., helper (n + 1) (i +. 1.) 1.0)
    else if n mod 2 = 0 then 
      StrCons (sum +. (1. /. (i *. -1.)), helper (n + 1) (i +. 1.) (sum +. (1. /. (i *. -1.))))
    else 
      StrCons (sum +. (1. /. i), helper (n + 1) (i +. 1.) (sum +. (1. /. i)))
  in
  helper 1 1.0 0.0
;;
