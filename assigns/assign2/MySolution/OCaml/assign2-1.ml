#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec
mylist_length(xs: 'a mylist): int = 

match xs with
  | MyNil -> 0
  | MyCons(x1, xs) -> 1 + mylist_length xs
  | MySnoc(xs, x1) -> 1 + mylist_length (MyReverse xs)
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> (mylist_length xs1) + (mylist_length xs2)
