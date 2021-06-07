# Ocaml-Interpreter
Takes in a string of commands and outputs them as inputs that either give information or act upon each other on a Stack
Commands: 
  Push ___ ; ___ = String, Int, Bool, or Unit
  Pop; Takes to value off of the stack
  Swap; Sawaps the two top vals of the stack
  Log; Prints the top val of the stack and pops it off of the stack
  Add; Adds the top two vals if they are both ints
  Sub; Subtracts the top two vals if they are both ints
  Mul; Multplies the top two vals if they are both ints
  Div; Divides the top two vals if they are both ints
  Rem; Give remainder of top two vals if they are both ints
  Neg; Gives negative version of top val if it is an int
  Cat; Return top val to the power of the second val to the stack
  And; Checks if top two vals on stack are the same
  Or; Checks if top two vals on stacka are different
  Not; Return the negation of the top val of the stack
  Eq; Checks if the top two vals of the stacka are equal
  Lte; Check if top val is less than 2nd val
  Lt; Check if top val is less than or equal to 2nd val
  Gte; Check if top val is greater than 2nd val
  Gt; Check if top val is greater than or equal to 2nd val
  Let; If top val is a name it regesters the name as a varible refering the then next val in the stack
  Ask; If given a name that is regesterd as a varible it return the value it referes to
  Begin (*commands*) End; Runs the commands inside of the begin end statement on an empty stack and return the top value of its local stack to the top of the global stack
  If Else End; Runs commands after If block and if true goes straight to the end block, if false runs the Else block
  Try Catch End; Runs commands in try block and if there are no errors go the end else run catch block
  Thow; Takes top val which if it is an int return it as an error
  DefFun __ __ End; first blank is name of fun, second black is a name to use as a varible, then all commands after that till end block are stored as as the function
  Call; If top val of stack is a name which goes with a function call the function and the next var in the stack after it becomes associated with the second name we had in the DefFun command.
