(* parser.ml *)

open Ast

(* Define a symbol table type *)
module SymbolTable = Hashtbl.Make(struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

(* Create a new symbol table and initialize with predefined symbols *)
let initialize_symbol_table () =
  let table = SymbolTable.create 50 in
  (* Add predefined symbols like R0, R1, ..., R15, SP, LCL, ARG, THIS, THAT, SCREEN, and KBD *)
  List.iter (fun (symbol, address) -> SymbolTable.add table symbol address)
    [
      ("R0", 0); ("R1", 1); ("R2", 2); ("R3", 3); ("R4", 4);
      ("R5", 5); ("R6", 6); ("R7", 7); ("R8", 8); ("R9", 9);
      ("R10", 10); ("R11", 11); ("R12", 12); ("R13", 13); ("R14", 14); ("R15", 15);
      ("SP", 0); ("LCL", 1); ("ARG", 2); ("THIS", 3); ("THAT", 4);
      ("SCREEN", 16384); ("KBD", 24576)
    ];
  table

(* Parse an A-instruction: @value or @symbol *)
let parse_a_instruction line symbol_table =
  try
    let value = int_of_string (String.sub line 1 (String.length line - 1)) in
    At value  (* If it's a number, use it directly *)
  with Failure _ -> 
    (* If it's not a number, treat it as a symbol and look it up in the symbol table *)
    let symbol = String.sub line 1 (String.length line - 1) in
    try
      let address = SymbolTable.find symbol_table symbol in
      At address
    with Not_found -> failwith ("Undefined symbol: " ^ symbol)

(* Parse a C-instruction *)
let parse_c_instruction line =
  (* Split the line into destination and computation+jump parts, if '=' is present *)
  let dest, comp_jump =
    match String.split_on_char '=' line with
    | [comp_jump] -> (None, comp_jump)  (* No destination *)
    | [dest; comp_jump] -> (Some dest, comp_jump)
    | _ -> failwith "Invalid line format: Too many '=' characters"
  in
  (* Split the computation+jump part further if ';' is present *)
  let comp, jump =
    match String.split_on_char ';' comp_jump with
    | [comp] -> (comp, None)  (* No jump *)
    | [comp; jump] -> (comp, Some jump)
    | _ -> failwith "Invalid line format: Too many ';' characters"
  in
  (dest, comp, jump)

(* Convert parsed string components to an AST C-instruction *)
let convert_to_c_instruction (dest, comp, jump) =
  (* Convert destination to register list and wrap it as a destination option *)
  let dest_registers =
    match dest with
    | None -> None  (* No destination specified *)
    | Some d -> 
      let reg_list = List.map (function
        | "A" -> A | "D" -> D | "M" -> M
        | _ -> failwith "Invalid register in destination"
      ) (String.split_on_char ',' d) in
      Some reg_list  (* Return as `Some` register list *)
  in
  (* Convert computation to output type *)
  let comp_output =
    match comp with
    | "0" -> Constant Zero
    | "1" -> Constant One
    | "-1" -> Constant MinusOne
    | "D" -> UApply (ID, D)
    | "A" -> UApply (ID, A)
    | "M" -> UApply (ID, M)
    | "!D" -> UApply (BNeg, D)
    | "!A" -> UApply (BNeg, A)
    | "!M" -> UApply (BNeg, M)
    | "-D" -> UApply (UMinus, D)
    | "-A" -> UApply (UMinus, A)
    | "-M" -> UApply (UMinus, M)
    | "D+1" -> BApply (Add, D, A)
    | "D-1" -> BApply (Sub, D, A)
    | "D+A" -> BApply (Add, D, A)
    | "D-A" -> BApply (Sub, D, A)
    | "D&M" -> BApply (And, D, M)
    | "D|M" -> BApply (Or, D, M)
    | _ -> failwith "Invalid computation expression"
  in
  (* Convert jump instruction if provided *)
  let jump_inst =
    match jump with
    | None -> None
    | Some "JGT" -> Some JGT
    | Some "JEQ" -> Some JEQ
    | Some "JGE" -> Some JGE
    | Some "JLT" -> Some JLT
    | Some "JNE" -> Some JNE
    | Some "JLE" -> Some JLE
    | Some "JMP" -> Some JMP
    | _ -> failwith "Invalid jump instruction"
  in
  C (dest_registers, comp_output, jump_inst)

(* Parse a single line of Hack assembly *)
let parse_line (line: string) symbol_table =
  let trimmed = String.trim line in
  if String.length trimmed = 0 || String.get trimmed 0 = '(' then
    None  (* Ignore labels and empty lines *)
  else if String.get trimmed 0 = '@' then
    Some (parse_a_instruction trimmed symbol_table)  (* A-instruction *)
  else
    let dest, comp, jump = parse_c_instruction trimmed in
    Some (convert_to_c_instruction (dest, comp, jump))  (* C-instruction *)

(* Read input lines and parse them into instructions *)
let parse_input (input: string list) symbol_table: 'v inst list =
  List.filter_map (fun line -> parse_line line symbol_table) input

