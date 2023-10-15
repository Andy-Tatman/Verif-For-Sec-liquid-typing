Function ::=
    "/*"    Variable ":" Type => Type   "*/"
    "main" "=" "\" Variable "." "{"    Statement*   Expr    "}" 

- The var in \ Variable . MUST equal the var in /* Variable ... */

// Might be nice to have a return value?
Statement ::= let Variable:Type = Expr ";" | Expr ";"
Expr ::= Const Int | Variable | Expr BinOp Expr | UnaryOp Expr
Variable ::= Letter { Letter | Digit }*
BinOp ::= "+" | "-" | "*" | "/" | "%"
UnaryOp ::= "-" 

Type ::= Int "{"    Refinement   "}"
Refinement ::= Variable | Pred
Pred ::= Pred "&&" Pred | Pred "||" Pred | !Pred | ConstB True | ConstB False | Expr CompOp Expr  
CompOp ::= "<=" | "=>" | "==" 
// Maybe uneq^


* ----------------------------------------------------------------------------
Example Main:

Positive Ex: /* x:Int{v>=0} => Int:{v>0} */ main = \x. { x+1 }
Negative Ex: /* x:Int{True} => Int:{v>0} */ main = \x. { x+1 }

main = \x. {

};


Example Main 2:

Positive Ex: /* x:Int{v>=0} => Int:{v<=0} */ main = \x. { let y = -x; y}
Negative Ex: /* x:Int{v>=0} => Int:{v>=0} */ main = \x. { let y = -x; y}