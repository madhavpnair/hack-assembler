open Ast

type minst = bytes

let const_minst(constant:const):minst = 
        match constant with 
        | Zero -> Bytes.of_string (String.make 1 (Char.chr 0b0101010))
        |One->Bytes.of_string(String.make 1(Char.chr 0b0111111))
        |MinusOne->Bytes.of_string(String.make 1(Char.chr 0b0111010))


let unary_minst (unary_op, reg: unary * register): minst =
    let code=
        match (unary_op, reg) with
        | (ID, D) ->0b0001100
        | (ID, A) -> 0b0110000
        | (ID, M) -> 0b1110000
        | (BNeg, D) ->0b0001101
        | (BNeg, M) ->0b1110001
        | (BNeg, A) -> 0b0110001
        | (UMinus, D) ->0b0001111
        | (UMinus, M) ->0b1110011
        | (UMinus, A) ->0b0110011
        | (Incr,D) -> 0b0011111
        | (Incr,A) -> 0b0110011
        | (Incr,M) -> 0b1110111
        | (Decr,D) -> 0b0001110
        | (Decr,A) -> 0b0110010
        | (Decr,M) -> 0b1110011
     in
     Bytes.of_string(String.make 1(Char.chr code))


let binary_op_to_minst ((bin_op, operand1, operand2): binary * register * register): minst =
    let code=
        match (bin_op, operand1, operand2) with
      
        | (Add, D, A) -> 0b0000010
        | (Add, D, M) -> 0b1000010
      
        | (Sub, D, A) -> 0b0010011
        | (Sub, D, M) -> 0b1010011
      
        | (And, D, A) -> 0b0000000
        | (And, D, M) -> 0b1000000
      
        | (Or, D, A) -> 0b0010101  
        | (Or, D, M) -> 0b1010101
  
        | _ -> failwith "Invalid binary operation or registers" 
     in
     Bytes.of_string(String.make 1(Char.chr code))



let dest_minst (reg_list: register list): minst =
  let code = 
    match reg_list with
    | [] -> 0b000               
    | [A] -> 0b100              
    | [D] -> 0b010              
    | [M] -> 0b001              
    | [A; M] -> 0b101  
    | [A; D]  -> 0b110  
    | [M; D]  -> 0b011  
    | [A; M; D]-> 0b111 
    | _ -> failwith "Invalid destination register combination"
  in
  Bytes.of_string (String.make 1 (Char.chr code))

let jump_minst(jump_inst:jump option):minst=
	let code=
	match jump_inst with
	|None->0b000
	|Some JGT->0b001
	|Some JEQ->0b010
	|Some JGE->0b011
	|Some JLT->0b100
	|Some JNE->0b101
	|Some JLE->0b110
	|Some JMP->0b111

	in
	Bytes.of_string(String.make 1(Char.chr code))


let a_instruction_to_minst(address:int):minst=
	let a_inst=0b0 lsl 15 lor address in
	Bytes.of_string (Printf.sprintf "%c%c" (Char.chr(a_inst lsr 8)) (Char.chr (a_inst land 0xFF)))


let c_instruction_to_minst (dest, comp, jump: destination * output * jump option): minst =
  let comp_code =
    match comp with
    | Constant c -> const_minst c
    | UApply (uop, reg) -> unary_minst (uop, reg)
    | BApply (bop, reg1, reg2) -> binary_op_to_minst (bop, reg1, reg2)
  in

  let dest_code = dest_minst dest in
  let jump_code = jump_minst jump in

  (* Create the full instruction as an integer *)
  let header = 0b111 in  (* Header for C-instructions *)
  let comp_bits = Char.code (Bytes.get comp_code 0) in
  let dest_bits = Char.code (Bytes.get dest_code 0) in
  let jump_bits = Char.code (Bytes.get jump_code 0) in
  
  (* Construct the full instruction *)
  let instruction = 
    (header lsl 13) lor
    (comp_bits lsl 6) lor
    (dest_bits lsl 3) lor
    jump_bits
  in

  (* Convert the instruction into a 2-byte bytes representation *)
  let byte1 = (instruction lsr 8) land 0xFF in  (* High byte *)
  let byte2 = instruction land 0xFF in           (* Low byte *)

  (* Create the bytes from the two bytes *)
  Bytes.of_string (String.make 1 (Char.chr byte1) ^ String.make 1 (Char.chr byte2))


	
let instr_to_minst (instr: 'v inst): minst =
  match instr with
  | At address -> a_instruction_to_minst address
  | C (dest_opt, comp, jump_opt) ->
      (* Unwrap `dest_opt` to get the register list *)
      let dest = match dest_opt with
        | Some d -> d   (* Use the register list inside `Some` *)
        | None -> []    (* Default to an empty register list if `None` *)
      in
      
      c_instruction_to_minst (dest, comp, jump_opt)
  |Label _->Bytes.empty

	
