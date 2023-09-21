#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;


let rec find_subseq index xs prev =
  if index = (string_length xs) -1 then
    if digit_of_char(string_get_at xs index) >= prev then
      str(string_get_at xs index)
    else 
      ""
  else if digit_of_char(string_get_at xs index) >= prev then
    string_cons (string_get_at xs index)(find_subseq (index + 1) xs (digit_of_char(string_get_at xs index)))
  else
    find_subseq (index + 1) xs (digit_of_char(string_get_at xs index))

let rec indexLoop n index max xs =
  if index < n -1 then
    if string_length(find_subseq index xs 0 ) < string_length(find_subseq (index + 1) xs 0) then
      indexLoop n (index + 1) (index + 1) xs
    else
      indexLoop n (index + 1) max xs
  else
    if string_length(find_subseq index xs 0) < string_length(find_subseq max xs 0) then
      find_subseq max xs 0
    else
      find_subseq index xs 0


let string_longest_ascend(xs: string): string =
  indexLoop (string_length xs) 0 0 xs

  let () = print_string(string_longest_ascend "1324561111")
  let () =
  assert(string_longest_ascend "1324561111" = "13456");;
  let () =
  assert(string_longest_ascend "1234561111" = "123456");;
  let () =
  assert(string_longest_ascend "1234511111" = "111111");;
  (* ****** ****** *)
  let () =
  print_string("Assign1-5-test passed!\n");;