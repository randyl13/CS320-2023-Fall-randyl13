#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let rec check_middle2 left right cs index = 
  if index = right then false
  else if (string_get_at cs index < string_get_at cs right) && (string_get_at cs right > string_get_at cs left ) && (string_get_at cs index < string_get_at cs left)
      then true
  else check_middle2 left right cs (index + 1)

let rec check_middle left right cs index = 
if index = right -1 then false
else if (string_get_at cs index < string_get_at cs right) && (string_get_at cs right > string_get_at cs left )
    then if check_middle2 index right cs (index + 1) then true
    else check_middle left right cs (index + 1)
else check_middle left right cs (index + 1)

let rec indexLoop left right cs =
  if right < string_length cs then
    if string_get_at cs left < string_get_at cs right then
      if check_middle left right cs (left + 1) then 
        false
      else 
        indexLoop left (right + 1) cs
    else
      indexLoop left (right + 1) cs
  else true

let rec myLoop left cs = 
  if left <= (string_length cs) - 4 then
    indexLoop left (left + 3) cs
  else
    myLoop (left + 1) cs

let string_avoid_1324(cs: string): bool = 
    indexLoop 0 2 cs
