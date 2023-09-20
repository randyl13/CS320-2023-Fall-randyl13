#use "./../../../classlib/OCaml/MyOcaml.ml";;
let i1 = 0;;
let i2 = 0;;


let lowercase_letters_work work_function =
  for c = Char.code 'a' to Char.code 'z' do
    work_function (Char.chr c)
    
  for c = Char.code 'a' to Char.code 'z' do
    i1 + 1
  done
;;

let lowercase_alphabet = string_make_fwork lowercase_letters_work