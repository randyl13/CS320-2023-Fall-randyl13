#use "./../../assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream = fun () ->
  let rec dfs_helper queue =
    match queue with
    | [] -> StrNil
    | GTnil :: rest -> dfs_helper rest
    | GTcons (x, children) :: rest ->
      let children_stream = children in
      let new_queue = list_append children_stream rest in
      StrCons (x, fun () -> dfs_helper new_queue)
  in
  dfs_helper [xs]
;;

let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream = fun () ->
  let rec bfs_helper queue =
    match queue with
    | [] -> StrNil
    | GTnil :: rest -> bfs_helper rest
    | GTcons (x, children) :: rest ->
      let children_stream = list_append rest children  in
      StrCons (x, fun () -> bfs_helper children_stream)
  in
  bfs_helper [xs]
;;