#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* abstract syntax tree of interp1 *)
type const =
  | Int of int
  | Bool of bool
  | Unit
  | Sym of string
  | Closure of string * (string * const) list * com list


and com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt | IfElse of coms * coms| Bind | Lookup
  | Fun of coms | Call | Return
and coms = com list


(* ------------------------------------------------------------ *)

(* parsers for interp1 *)
let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))

let parse_unit =
  keyword "Unit" >> pure Unit

let char_or_digit =
  satisfy (fun c -> char_isalphanum c || char_isdigit c)

let parse_sym =
  let* chars_and_digits = many char_or_digit in
  pure (Sym (string_make_fwork (list_foreach chars_and_digits)))


let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_sym 

let remove_whitespace (s : string) : string =
  let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  string_filter s (fun c -> not (is_whitespace c))

let parse_com2 = 
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt) <|>
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) <|>
  (keyword "Call" >> pure Call) <|>
  (keyword "Return" >> pure Return)

let parse_com = 
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt) <|>
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) <|>
  (keyword "Call" >> pure Call) <|>
  (keyword "Return" >> pure Return) <|>
  (keyword "If" >>
    many (parse_com2 << keyword ";") >>= fun c1 ->
    keyword "Else" >>
    many (parse_com2 << keyword ";") >>= fun c2 ->
    keyword "End" >>
    pure (IfElse (c1, c2))) <|>
  (keyword "Fun" >>
    many (parse_com2 << keyword ";") >>= fun c3 ->
    keyword "End" >>
    pure (Fun c3))

let parse_coms = many (parse_com << keyword ";")

(* ------------------------------------------------------------ *)

(* interpreter *)

type stack = const list
type trace = string list
type variable = (string * const) list
type prog = coms

let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n

let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Unit -> "Unit"
  | Sym s -> s

let rec list_exists xs c =
  match xs with
  | [] -> false
  | (x, _) :: xs' -> x = c || list_exists xs' c

let rec list_lookup xs c =
  match xs with
  | [] -> failwith "Variable not found"
  | (x, v) :: xs' -> if x = c then v else list_lookup xs' c

let rec eval (s : stack) (t : trace) (v : variable) (p : prog) : trace =
  match p with
  (* termination state returns the trace *)
  | [] -> t
  | Push c :: p0 (* PushStack *) -> eval (c :: s) t v p0
  | Pop :: p0 ->
    (match s with
     | _ :: s0 (* PopStack *) -> eval s0 t v p0
     | []      (* PopError *) -> eval [] ("Panic" :: t) [] [])
  
  | Swap :: p0 ->
    (match s with
     | i :: j :: s0         (* SwapStack *)  -> eval (j :: i :: s0) t v p0
     | []                   (* SwapError1 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* SwapError2 *) -> eval [] ("Panic" :: t) [] [])

  | Trace :: p0 ->
    (match s with
     | c :: s0 (* TraceStack *) -> eval (Unit :: s0) (toString c :: t) v p0
     | []      (* TraceError *) -> eval [] ("Panic" :: t) [] [])
  | Add :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* AddStack *)  -> eval (Int (i + j) :: s0) t v p0
     | _ :: _ :: s0         (* AddError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) [] [])
  | Sub :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* SubStack *)  -> eval (Int (i - j) :: s0) t v p0
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) [] [])
  | Mul :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* MulStack *)  -> eval (Int (i * j) :: s0) t v p0
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) [] [])
  | Div :: p0 ->
    (match s with
     | Int i :: Int 0 :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) [] []
     | Int i :: Int j :: s0 (* DivStack *)  -> eval (Int (i / j) :: s0) t v p0
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) [] [])
  | And :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* AndStack *)  -> eval (Bool (a && b) :: s0) t v p0
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) [] []
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) [] [])
  | Or :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* OrStack *)  -> eval (Bool (a || b) :: s0) t v p0
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) [] []
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) [] [])
  | Not :: p0 ->
    (match s with
     | Bool a :: s0 (* NotStack  *) -> eval (Bool (not a) :: s0) t v p0
     | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) [] []
     | []           (* NotError2 *) -> eval [] ("Panic" :: t) [] [])
  | Lt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* LtStack *)  -> eval (Bool (i < j) :: s0) t v p0
     | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* LtError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) [] [])
  | Gt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* GtStack *)  -> eval (Bool (i > j) :: s0) t v p0
     | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* GtError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) [] [])

  | Bind :: p0 ->
    (match s with
     | Sym i :: j :: s0     (* BindStack *)  -> eval s0 t ((i, j) :: v) p0
     | _ :: _ :: s0         (* BindError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* BindError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* BindError3 *) -> eval [] ("Panic" :: t) [] [])
  
  | Lookup :: p0 ->
    (match s with
     | Sym i :: s0          (* LookupStack *)  -> 
        if list_exists v i then 
          eval (list_lookup v i :: s0) t v p0 
        else 
          eval [] ("Panic" :: t) [] []
     | _ :: []              (* LookupError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* LookupError2 *) -> eval [] ("Panic" :: t) [] [])

  | IfElse (c1, c2) :: p0 ->
    (match s with
    | Bool b :: s0 ->
      if b then
        eval s0 t v (list_append c1 p0) 
      else
        eval s0 t v (list_append c2 p0)  
    | _ :: []              (* IfElseError1 *) -> eval [] ("Panic" :: t) [] []
    | []                   (* IfElseError2 *) -> eval [] ("Panic" :: t) [] [])
  
  | Fun (c1) :: p0 ->
    (match s with
    | Sym i :: s0 -> eval  (Closure (i, v, c1) :: s0) t v p0
    | _ :: []              (* FunError1 *) -> eval [] ("Panic" :: t) [] []
    | []                   (* FunError2 *) -> eval [] ("Panic" :: t) [] [])

  | Call :: p0 ->
    (match s with
    | Closure (f, env, closure_commands) :: a :: s0 (* CallStack *) ->
      let cc = "cc" in
      let updated_env = (f, Closure (f, env, closure_commands)) :: env in
      eval (a :: Closure (cc, updated_env, p0) :: s0) t updated_env closure_commands
    | _ :: s0                   (* CallError1 *) -> eval [] ("Panic" :: t) [] []
    | []                        (* CallError2 *) -> eval [] ("Panic" :: t) [] [])

  | Return :: p0 ->
  (match s with
   | Closure (f, env, closure_commands) :: a :: s0 (* ReturnStack *) ->
     eval (a :: s0) t env closure_commands
   | _ :: s0                   (* ReturnError1 *) -> eval [] ("Panic" :: t) [] []
   | []                        (* ReturnError2 *) -> eval [] ("Panic" :: t) [] [])

(* ------------------------------------------------------------ *)

(* putting it all together [input -> parser -> eval -> output] *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms) (remove_whitespace s) with
  | Some (p, []) -> Some (eval [] [] [] p)
  | _ -> None

(* ------------------------------------------------------------ *)

(* interp from file *)

let read_file (fname : string) : string =
  let fp = open_in fname in
  let s = string_make_fwork (fun work ->
      try
        while true do
          work (input_char fp)
        done
      with _ -> ())
  in
  close_in fp; s

let interp_file (fname : string) : string list option =
  let src = read_file fname in
  interp src


let program_tokens2 = string_parse (whitespaces >> parse_coms)(remove_whitespace "Push factorial;
Fun
Push n;
Bind;
Push n;
Lookup;
Push 2;
Gt;
If
Push 1;
Swap;
Return;
Else
Push n;
Lookup;
Push -1;
Add;
Push factorial;
Lookup;
Call;
Push n;
Lookup;
Mul;
Swap;
Return;
End;
End;
Push factorial;
Bind;
Push 4;
Push factorial;
Lookup;
Call;
Trace;")

let program_tokens1 = string_parse (whitespaces >> parse_coms) "Push poly;
Fun
Push x;
Bind;
Push x;
Lookup;
Push x;
Lookup;
Mul;
Push -4;
Push x;
Lookup;
Mul;
Add;
Push 7;
Add;
Swap;
Return;
End;
Push 3;
Swap;
Call;
Trace;"

let () =
  let test_case program =
    match interp program with
    | Some result -> Printf.printf "Program: %s\nResult: %s\n\n" program (String.concat "; " result)
    | None -> Printf.printf "Program: %s\nResult: None\n\n" program
  in

  test_case "Push factorial;
Fun
Push n;
Bind;
Push n;
Lookup;
Push 2;
Gt;
If
Push 1;
Swap;
Return;
Else
Push n;
Lookup;
Push -1;
Add;
Push factorial;
Lookup;
Call;
Push n;
Lookup;
Mul;
Swap;
Return;
End;
End;
Push factorial;
Bind;
Push 4;
Push factorial;
Lookup;
Call;
Trace;";

