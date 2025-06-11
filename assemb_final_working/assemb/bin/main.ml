open Parser
open Machine
(*open Bytes*)

let read_lines () =
  let rec read_lines_aux acc =
    try
      let line = read_line () in
      read_lines_aux (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  read_lines_aux []

(* Manually convert a byte to a binary string *)
let byte_to_binary_string (b: bytes) : string =
  let result = ref "" in
  for i = 0 to Bytes.length b - 1 do
    let byte_val = Char.code (Bytes.get b i) in
    for j = 7 downto 0 do
      let bit = (byte_val lsr j) land 1 in
      result := !result ^ string_of_int bit
    done
  done;
  (* If the result is shorter than 16 bits, pad it *)
  let len = String.length !result in
  if len < 16 then String.make (16 - len) '0' ^ !result else !result

let () =
  (* Read the entire assembly input *)
  let lines = read_lines () in

  (* Parse the program using the parser *)
  let parsed_instructions = parse_program lines in

  (* Convert each instruction to machine code and print the 16-bit binary strings *)
  List.iter
    (fun instruction ->
      let machine_code = instr_to_minst instruction in
      let binary_string = byte_to_binary_string machine_code in
      if binary_string <> "" then print_endline binary_string
    )
    parsed_instructions

