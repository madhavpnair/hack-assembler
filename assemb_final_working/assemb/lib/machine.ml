open Ast

type minst = bytes

let const_minst(constant:const):minst = 
        match constant with 
        | Zero -> Bytes.of_string (String.make 1 (Char.chr 0b0101010))
        |One->Bytes.of_string(String.make 1(Char.chr 0b0111111))
        |MinusOne->Bytes.of_string(String.make 1(Char.chr 0b0111010))


let unary_minst (unary_op, reg: unary * register): minst =
    match (unary_op, reg) with
    | (ID, D) -> Bytes.of_string (String.make 1(Char.chr 0b0001100))
    | (ID, A) -> Bytes.of_string (String.make 1(Char.chr 0b0110000))
    | (ID, M) -> Bytes.of_string (String.make 1(Char.chr 0b1110000))
    | (BNeg, D) -> Bytes.of_string (String.make 1(Char.chr 0b0001101))
    | (BNeg, M) -> Bytes.of_string (String.make 1(Char.chr 0b1110001))
    | (BNeg, A) -> Bytes.of_string (String.make 1(Char.chr 0b0110001))  
    | (UMinus, D) -> Bytes.of_string (String.make 1(Char.chr 0b0001111))
    | (UMinus, M) -> Bytes.of_string (String.make 1(Char.chr 0b1110011))
    | (UMinus, A) -> Bytes.of_string (String.make 1(Char.chr 0b0110011)) 
    | (Incr,D) -> Bytes.of_string (String.make 1(Char.chr 0b0011111))  
    | (Incr,A) -> Bytes.of_string (String.make 1(Char.chr 0b0110011))
    | (Incr,M) -> Bytes.of_string (String.make 1(Char.chr 0b1110011))  
    | (Decr,D) -> Bytes.of_string (String.make 1(Char.chr 0b0001110))  
    | (Decr,A) -> Bytes.of_string (String.make 1(Char.chr 0b0110010)) 
    | (Decr,M) -> Bytes.of_string (String.make 1(Char.chr 0b1110011))  
(*    | _ -> failwith "Invalid unary operation or register" *)  




let binary_op_to_minst ((bin_op, operand1, operand2): binary * register * register): minst =
    match (bin_op, operand1, operand2) with
      
    | (Add, D, A) -> Bytes.of_string (String.make 1(Char.chr 0b0000010))  
    | (Add, D, M) -> Bytes.of_string (String.make 1(Char.chr 0b1000010))  
      
    | (Sub, D, A) -> Bytes.of_string (String.make 1(Char.chr 0b0010011))  
    | (Sub, D, M) -> Bytes.of_string (String.make 1(Char.chr 0b1010011))  
      
    | (And, D, A) -> Bytes.of_string (String.make 1(Char.chr 0b0000000))  
    | (And, D, M) -> Bytes.of_string (String.make 1(Char.chr 0b0000000))
      
    | (Or, D, A) -> Bytes.of_string (String.make 1(Char.chr 0b0000011))   
    | (Or, D, M) -> Bytes.of_string (String.make 1(Char.chr 0b0000011)) 
  
    | _ -> failwith "Invalid binary operation or registers" 



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
(*	|_->failwith "Invalid jump instruction"*)
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
    | BApply (bop, reg1,reg2) -> binary_op_to_minst(bop, reg1, reg2)
  in
  let dest_code = dest_minst dest in
  let jump_code = jump_minst jump in
  Bytes.cat (Bytes.of_string (String.make 1 (Char.chr 0b11100000))) (Bytes.concat Bytes.empty [comp_code; dest_code; jump_code])
	


	
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

	
(*let encode(inst:inst):minst = 
        match inst with
        |At of 'v
        |C of destination*output*jump option
           let dest_code = 
                   match destination with
                   |
                   |M->001
                   |D->010
                   |MD->011
                   |A->100
                   |AM->101
                   |AD->110
                   |AMD->111
           let output_code =
                   match output with
                   |Constant of const
                   |UApply of unary
                   |BApply of binary
                      
                      let unary_code = 
                              match UApply wit
*)
