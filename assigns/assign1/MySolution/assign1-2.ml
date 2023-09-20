#use "./../../../classlib/OCaml/MyOcaml.ml";;

(*function rec that increases index by one and checks the current index and compares and chooses
 until string is complete*)

let rec find_index i0 i1 i2 cs1 cs2=
if i1 < string_length cs1 && i2 < string_length cs2 then
  if i1 + i2 + 2 < i0 + 1 then
    if string_get_at cs1 i1 <= string_get_at cs2 i2 then
      find_index i0 (i1 + 1) i2 cs1 cs2
    else 
      find_index i0 i1 (i2 + 1) cs1 cs2
  else 
    if string_get_at cs1 i1 <= string_get_at cs2 i2 then
      string_get_at cs1 i1
    else 
      string_get_at cs2 i2
else if i1 < string_length cs1 then
  if i1 + i2 + 2 < i0 + 1 then
    find_index i0 (i1 + 1) i2 cs1 cs2
  else
    string_get_at cs1 i1
else
  if i1 + i2 + 2 < i0 + 1 then
    find_index i0 i1 (i2 + 1) cs1 cs2
  else
    string_get_at cs2 i2
    ;;

let merge_strings cs1 cs2 work=
    for i0 = 1 to ((string_length cs1) + (string_length cs2)) do
      work (find_index i0 0 0 cs1 cs2)
    done
  ;;

let string_merge (cs1: string) (cs2: string): string =
  if cs1 = "" then cs2
  else if cs2 = "" then cs1
  else
  string_make_fwork (merge_strings cs1 cs2)
;;
