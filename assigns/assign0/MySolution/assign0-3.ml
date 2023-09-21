(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

(* ****** This function converts the given int i0 into a string
representation by first finding the length of the int (num of numbers)
and a function that maps each int element (in chr form) to the appropriate
index ****** *)

let rec chrLength i0 acc=
  if i0 = 0 then acc
  else
    chrLength (i0 / 10)(acc + 1)

let int2str(i0: int): string =

  let rec chrDigit i0 i : char=
    
    let num = i0 mod 10 in
    let last_digit = chr(ord('0') + num) in
    if i = 0 then last_digit
    else chrDigit (i0 / 10) (i - 1)
      
  in
  if i0 = 0 then "0"
  else if i0 < 0 then
    let int_length = chrLength i0 0 in
    string_init ((chrLength i0 0) + 1) (fun i -> 
      if i = int_length then '-'
      else 
      chrDigit i0 (int_length - i -1))
  else
  let int_length = chrLength i0 0 in
  string_init (chrLength i0 0) (fun i -> chrDigit i0 (int_length - i -1))
;;


