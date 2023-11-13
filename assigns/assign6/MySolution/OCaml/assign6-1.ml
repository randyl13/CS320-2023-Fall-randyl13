#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let string_append(xs: string)(ys: string): string =
  string_make_fwork(
    fun work -> (string_foreach xs work; string_foreach ys work)
  )
;;

let rec list_map(fopr)(xs) =
match xs with
| [] -> []
| x1 :: xs -> fopr(x1) :: list_map(fopr)(xs)

let rec string_concat(sep: string)(strings : string list) : string =
  match strings with
  | [] -> ""
  | [s] -> s
  | s :: ss -> s ^ sep ^ string_concat sep ss

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)
(* turn a string into a list of chars *)

let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

let string_of_list (cs) =
  string_make_fwork(fun work -> list_foreach cs work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs


type 'a parser = char list -> ('a * char list) option

let parse_digit (cs : char list) : (int * char list) option =
  match cs with
  | [] -> None
  | c :: cs ->
    let i = ord c - ord '0' in 
    if 0 <= i && i <= 9
    then Some (i, cs)
    else None

let rec many (p : 'a parser) (cs : char list) : ('a list * char list) option =
  match p cs with
  | Some (x, cs0) ->
    (match many p cs0 with
     | Some (xs, cs1) -> Some (x :: xs, cs1)
     | None -> Some ([x], cs0))
  | None -> Some ([], cs)

let rec many1 (p : 'a parser) (cs : char list) : ('a list * char list) option =
  match p cs with
  | Some (x, cs0) ->
    (match many p cs0 with
     | Some (xs, cs1) -> Some (x :: xs, cs1)
     | None -> None)
  | None -> None

let int_of_list xs =
  let rec loop xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> loop xs (acc * 10 + x)
  in
  loop xs 0

let parse_num (cs : char list) : (int * char list) option =
  match many1 parse_digit cs with
  | Some (xs, cs0) -> Some (int_of_list xs, trim cs0)
  | None -> None

let parse_word (s : string) (cs : char list) : (unit * char list) option =
  let cs0 = string_listize s in
  let rec loop cs cs0 =
    match cs, cs0 with
    | _, [] -> Some ((), trim cs)
    | [], _ -> None
    | c :: cs', c0 :: cs0' ->
      if c = c0
      then loop cs' cs0'
      else None
  in
  loop cs cs0

let rec choice (ps : 'a parser list) (cs : char list) : ('a * char list) option = 
  match ps with
  | [] -> None
  | p :: ps0 ->
    match p cs with
    | Some (x, cs0) -> Some (x, cs0)
    | None -> choice ps0 cs

let rec parse_sexpr (cs : char list) : (sexpr * char list) option =
  choice [parse_int; parse_add; parse_mul] cs

and parse_int (cs : char list) : (sexpr * char list) option =
  match parse_num cs with
  | Some (i, cs0) -> Some (SInt i, cs0)
  | None -> None

and parse_add (cs : char list) : (sexpr * char list) option =
  match parse_word "(add" cs with
  | Some (_, cs0) -> 
    (match many1 parse_sexpr cs0 with
     | Some (es, cs1) ->
       (match parse_word ")" cs1 with
        | Some (_, cs2) -> Some (SAdd es, cs2)
        | None -> None)
     | None -> None)
  | None -> None

and parse_mul (cs : char list) : (sexpr * char list) option =
  match parse_word "(mul" cs with
  | Some (_, cs0) -> 
    (match many1 parse_sexpr cs0 with
     | Some (es, cs1) ->
       (match parse_word ")" cs1 with
        | Some (_, cs2) -> Some (SMul es, cs2)
        | None -> None)
     | None -> None)
  | None -> None

let sexpr_parse (s : string) : sexpr option =
  match parse_sexpr (string_listize s) with
  | Some (e, []) -> Some e
  | _ -> None

let rec sexpr_to_string (e : sexpr)  : string =
  match e with
  | SInt i -> str(char_of_digit i)
  | SAdd i -> let x = string_concat " " (list_map sexpr_to_string i) in
  let y = string_append "(add " x in
  string_append y ")"
  | SMul i -> let x = string_concat " " (list_map sexpr_to_string i) in
  let y = string_append "(mul " x in
  string_append y ")"

