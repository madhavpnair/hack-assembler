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
        |BApply of binary*register*register

type destination = register list

type 'v inst = 
        |At of 'v
        |C of destination option * output * jump option
