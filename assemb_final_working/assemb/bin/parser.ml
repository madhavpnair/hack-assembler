open Ast


(* Symbol table to store predefined symbols, labels, and variables using Hashtbl library*)
let symbol_table : (string, int) Hashtbl.t = Hashtbl.create 100

(* Initialize the predefined symbols *)
let initialize_predefined_symbols () =
  (* Predefined register symbols *)
  for i = 0 to 15 do
    Hashtbl.add symbol_table (Printf.sprintf "R%d" i) i
  done;
  (* Other predefined symbols *)
  Hashtbl.add symbol_table "SP" 0;
  Hashtbl.add symbol_table "LCL" 1;
  Hashtbl.add symbol_table "ARG" 2;
  Hashtbl.add symbol_table "THIS" 3;
  Hashtbl.add symbol_table "THAT" 4;
  Hashtbl.add symbol_table "SCREEN" 16384;
  Hashtbl.add symbol_table "KBD" 24576

(* Add label to the symbol table *)
let add_label (label: string) (address: int) =
  if not (Hashtbl.mem symbol_table label) then
    Hashtbl.add symbol_table label address

(* Parse destination part of the instruction *)
let parse_destination (dest: string option) : destination option =
  match dest with
  | None -> None
  | Some s ->
    let reg_list = ref [] in
    if String.contains s 'A' then reg_list := A :: !reg_list;
    if String.contains s 'D' then reg_list := D :: !reg_list;
    if String.contains s 'M' then reg_list := M :: !reg_list;
    Some !reg_list

(* Parse computation part of the instruction *)
let parse_computation (comp: string) : output =
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
  | "D+1" -> UApply (Incr, D)
  | "A+1" -> UApply (Incr, A)
  | "M+1" -> UApply (Incr, M)
  | "D-1" -> UApply (Decr, D)
  | "A-1" -> UApply (Decr, A)
  | "M-1" -> UApply (Decr, M)
  | "D+A" -> BApply (Add, D, A)
  | "D+M" -> BApply (Add, D, M)
  | "D-A" -> BApply (Sub, D, A)
  | "D-M" -> BApply (Sub, D, M)
  | "A-D" -> BApply (Sub, A, D)
  | "M-D" -> BApply (Sub, M, D)
  | "D&A" -> BApply (And, D, A)
  | "D&M" -> BApply (And, D, M)
  | "D|A" -> BApply (Or, D, A)
  | "D|M" -> BApply (Or, D, M)
  | _ -> failwith "Invalid computation string"

(* Parse jump part of the instruction *)
let parse_jump (jump: string option) : jump option =
  match jump with
  | None -> None
  | Some "JGT" -> Some JGT
  | Some "JEQ" -> Some JEQ
  | Some "JGE" -> Some JGE
  | Some "JLT" -> Some JLT
  | Some "JLE" -> Some JLE
  | Some "JNE" -> Some JNE
  | Some "JMP" -> Some JMP
  | _ -> failwith "Invalid jump string"

(* Parse a single line of Hack assembly code *)
let parse_line (line: string) (current_address: int ref) (variable_address: int ref) : 'v inst option =
  let line = String.trim line in
  (* Remove comments *)
  let line =
    try String.sub line 0 (String.index line '/') |> String.trim
    with Not_found -> line
  in
  (* Skip empty lines *)
  if line = "" then None
  else
    match line.[0] with
    | '(' ->
      (* Parse label and add to the symbol table *)
      let label = String.sub line 1 (String.length line - 2) in
      add_label label !current_address;
      None
    | '@' ->
      (* Parse A-instruction *)
      let symbol = String.sub line 1 (String.length line - 1) in
      let address =
        try int_of_string symbol  (* Direct numeric address *)
        with Failure _ ->
          if Hashtbl.mem symbol_table symbol then Hashtbl.find symbol_table symbol
          else (
            (* Allocate a new address for variable *)
            Hashtbl.add symbol_table symbol !variable_address;
            incr variable_address;
            Hashtbl.find symbol_table symbol
          )
      in
      Some (At address)
    | _ ->
      (* Parse C-instruction *)
      let dest, comp, jump =
        try
          let eq_pos = String.index line '=' in
          let semi_pos = String.index line ';' in
          let dest = Some (String.sub line 0 eq_pos) in
          let comp = String.sub line (eq_pos + 1) (semi_pos - eq_pos - 1) in
          let jump = Some (String.sub line (semi_pos + 1) (String.length line - semi_pos - 1)) in
          dest, comp, jump
        with Not_found ->
          let eq_pos = try String.index line '=' with Not_found -> -1 in
          let dest = if eq_pos <> -1 then Some (String.sub line 0 eq_pos) else None in
          let comp =
            if eq_pos <> -1 then String.sub line (eq_pos + 1) (String.length line - eq_pos - 1)
            else String.sub line 0 (String.index line ';')
          in
          let jump = if String.contains line ';' then Some (String.sub line ((String.index line ';') + 1) (String.length line - (String.index line ';') - 1)) else None in
          dest, comp, jump
      in
      let dest = parse_destination dest in
      let comp = parse_computation comp in
      let jump = parse_jump jump in
      Some (C (dest, comp, jump))

(* Parse the entire assembly program into an AST *)
let parse_program (lines: string list) : 'v inst list =
  initialize_predefined_symbols ();
  let current_address = ref 0 in
  let variable_address = ref 8 in
  let instructions = ref [] in
  List.iter (fun line ->
    match parse_line line current_address variable_address with
    | Some instr ->
      instructions := !instructions @ [instr];
      incr current_address
    | None -> ()
  ) lines;
  !instructions

