Function ::=
    "/*"    Variable ":" Type => Type   "*/"
    "main" "=" "\" Variable "." "{"    Statement*   Expr    "}" 

- The var in \ Variable . MUST equal the var in /* Variable ... */

Statement ::= “let ” Variable ”:” Type = Expr ";" | Expr ";"
Expr ::= "(" Expr ")" | Const Int | Variable | Expr BinOp Expr | UnaryOp Expr | "if" Pred "then" Expr "else" Expr
Variable ::= Letter { Letter | Digit }*
BinOp ::= "+" | "-" | "*" | "/" | "%"
UnaryOp ::= "-" 

Type ::= "Int" "{"    Refinement   "}"
Refinement ::= Variable "|" Pred
Pred ::= "(" Pred ")" | Pred "&&" Pred | Pred "||" Pred | !Pred | ConstB True | ConstB False | Expr CompOp Expr  
CompOp ::= "<=" | "<" | "<=" | ">" | "==" | "!="


* ----------------------------------------------------------------------------
Example Main:

Positive Ex: /* x:Int{v|v>=0} => Int{v|v>0} */ main = \x. { x+1 }
Negative Ex: /* x:Int{v|True} => Int{v|v>0} */ main = \x. { x+1 }

Example Main 2:

Positive Ex: /* x:Int{v|v>=0} => Int{v|v<=0} */ main = \x. { let y:Int{v|v<=0} = -x; y}
Negative Ex: /* x:Int{v|v>=0} => Int{v|v>=0} */ main = \x. { let y:Int{v|v>=0} = -x; y}