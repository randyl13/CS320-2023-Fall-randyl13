#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let rec add index num1 num2 carry: string =
if index < string_length num1 && index < string_length num2 then 
  if carry then
    if (digit_of_char(string_get_at num1 (string_length num1 - index -1)) + digit_of_char(string_get_at num2 (string_length num2 - index -1))) + 1 > 9 then
      string_snoc(add (index + 1) num1 num2 true)(chr((1 + digit_of_char(string_get_at num1 (string_length num1 - index -1)) + digit_of_char(string_get_at num2 (string_length num2 - index -1))) mod 10 + 48))
    else 
      string_snoc(add (index + 1) num1 num2 false)(chr((1 + digit_of_char(string_get_at num1 (string_length num1 - index -1)) + digit_of_char(string_get_at num2 (string_length num2 - index -1))) mod 10 + 48))
  else 
    if (digit_of_char(string_get_at num1 (string_length num1 - index -1)) + digit_of_char(string_get_at num2 (string_length num2 - index -1))) > 9 then
      string_snoc(add (index + 1) num1 num2 true)(chr((digit_of_char(string_get_at num1 (string_length num1 - index -1)) + digit_of_char(string_get_at num2 (string_length num2 - index -1))) mod 10 + 48))
    else 
      string_snoc(add (index + 1) num1 num2 false)(chr((digit_of_char(string_get_at num1 (string_length num1 - index -1)) + digit_of_char(string_get_at num2 (string_length num2 - index -1))) mod 10 + 48))
else if index < string_length num1 then
  if carry then
    if (digit_of_char(string_get_at num1(string_length num1 - index -1))) +1 > 9 then
      string_snoc(add (index + 1) num1 num2 true) (chr((1 + digit_of_char(string_get_at num1 (string_length num1 - index -1))) mod 10 + 48))
    else 
      string_snoc(add (index + 1) num1 num2 false) (chr((1 + digit_of_char(string_get_at num1 (string_length num1 - index -1))) mod 10 + 48))
  else
    if (digit_of_char(string_get_at num1(string_length num1 - index -1))) > 9 then
      string_snoc(add (index + 1) num1 num2 true) (chr((digit_of_char(string_get_at num1 (string_length num1 - index -1))) mod 10 + 48))
    else 
      string_snoc(add (index + 1) num1 num2 false) (chr((digit_of_char(string_get_at num1 (string_length num1 - index -1))) mod 10 + 48))
else if index < string_length num2 then
  if carry then
    if (digit_of_char(string_get_at num2 (string_length num2 - index -1))) +1 > 9 then
      string_snoc(add (index + 1) num1 num2 true) (chr((1 + digit_of_char(string_get_at num2 (string_length num2 - index -1))) mod 10 + 48))
    else 
      string_snoc(add (index + 1) num1 num2 false) (chr((1 + digit_of_char(string_get_at num2 (string_length num2 - index -1))) mod 10 + 48))
  else
    if (digit_of_char(string_get_at num2 (string_length num2 - index -1))) > 9 then
      string_snoc(add (index + 1) num1 num2 true) (chr((digit_of_char(string_get_at num2 (string_length num2 - index -1))) mod 10 + 48))
    else 
      string_snoc(add (index + 1) num1 num2 false) (chr((digit_of_char(string_get_at num2 (string_length num2 - index -1))) mod 10 + 48))
else 
  if carry then
    "1"
  else
    ""

let intrep_add(ds1: string)(ds2: string): string = 
if ds1 = "" then ds2
else if ds2 = "" then ds1
else if string_length ds1 > string_length ds2 then
  add 0 ds1 ds2 false
else 
  add 0 ds2 ds1 false
;;

