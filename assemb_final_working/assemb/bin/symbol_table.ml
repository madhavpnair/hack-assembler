module StringMap = Map.Make(String)

type t = int StringMap.t

let create () : t = StringMap.empty

let add (table: t) (key: string) (value: int) : t =
  StringMap.add key value table

let add_symbol table key value =
  add table key value

let find (table: t) (key: string) : int option =
  StringMap.find_opt key table

let iter f (table: t) =
  StringMap.iter f table

let predefined_symbols () : t =
  let table = create () in
  let predefined = [
    ("SP", 0);
    ("LCL", 1);
    ("ARG", 2);
    ("THIS", 3);
    ("THAT", 4);
    ("R0", 0);
    ("R1", 1);
    ("R2", 2);
    ("R3", 3);
    ("R4", 4);
    ("R5", 5);
    ("R6", 6);
    ("R7", 7);
    ("R8", 8);
    ("R9", 9);
    ("R10", 10);
    ("R11", 11);
    ("R12", 12);
    ("R13", 13);
    ("R14", 14);
    ("R15", 15);
    ("SCREEN", 16384);
    ("KBD", 24576);
  ] in
  List.fold_left (fun acc (k, v) -> add acc k v) table predefined

let next_variable_address (table: t) =
  let rec find_next_address addr =
    if find table (string_of_int addr) = None then addr
    else find_next_address (addr + 1)
  in
  find_next_address 16

