type  jump  = 
	   |JGT
           |JEQ
           |JGE
           |JLT
           |JLE
           |JNE
           |JMP
           
	   

type const = Zero | One | MinusOne

type register = A | D | M

type unary = BNeg  | UMinus | ID |Incr| Decr

type binary = Add | Sub | And | Or

type output = 
        |Constant of const
        |UApply of unary*register
        |BApply of binary * register * register

type destination = register list

type 'v inst = 
        |At of 'v
        |C of destination option * output * jump option
        |Label of string

(*type 'v block='v inst list
type 'v program='v block list*)


       (* type address = int*)

       (* type 'v inst = At of 'v((*@x*)
             | JMP
             | JLT
             | JLE
             |*)
(*type 'v block = 'v inst list
type 'v lbloack = string * 'v block
type 'v program = 'v block list

let resolve (pg : string program) = ([] : address block)


(*
let resolveLabels (pg : string program) =
        ([] : (string * address) list) *)

let rec resolveLabels add = function
        | []  ->  []
        | (l,blk) :: rest  ->
          let newAdd = add + List.length blk in
          (l, add) :: resolveLabels newAdd rest*)
