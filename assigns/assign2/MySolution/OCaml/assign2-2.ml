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

let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a =
match xs with
  | MyNil -> mylist_subscript_exn()
  | MyCons(x1, xs) -> if i0 = 0 then x1 else mylist_get_at(xs)(i0 - 1)
  | MySnoc(xs, x1) -> if i0 = 0 then x1 else mylist_get_at(MyReverse xs)(i0 - 1)
  | MyReverse(xs) -> mylist_get_at(xs)(mylist_length xs - i0 -1)
  | MyAppend2(xs1, xs2) -> if i0 < mylist_length xs1 then mylist_get_at(xs1)(i0)
  else mylist_get_at(xs2)(i0 - (mylist_length xs1))
