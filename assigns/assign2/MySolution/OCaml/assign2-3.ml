
let rec
list_foldleft
(xs: 'a list)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| [] -> r0
| (x1 :: xs) ->
  list_foldleft(xs)(fopr(r0)(x1))(fopr)
;;

let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =

let foreach_to_foldright(foreach: ('xs, 'x0) foreach): 'xs -> 'r0 -> ('x0 -> 'r0 -> 'r0) -> 'r0 =
fun xs r0 fopr -> 
  let xs = foreach_to_rlistize(foreach)(xs) in
    list_foldleft(xs)(r0)(fun r0 x0 -> fopr x0 r0)