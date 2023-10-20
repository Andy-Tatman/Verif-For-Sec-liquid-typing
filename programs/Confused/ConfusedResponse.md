Per file, the following options:
1) Parse error. (Won't then be checked by SMT solver)
2) Invalid. (SMT solver says No)
3) Valid.
changeTypeVar.txt: 
Invalid. It parses fine, but the y in Int{v | y > 0} is free, so this is equiv. to (\forall y. y > 0) which is equiv to True. 

conj.txt:
Valid. Conj and disj are fine, and 0 == 0.

constIntType.txt:
Parse error.
In x:Int{v | 0}, 0 is not a predicate. The predicate must be a boolean expression.

extraEnter.txt:
(Removing the // to parse it.)
Parse error. The last part of the body is not an expression. 
Further parse errors: "Int{v>0}" should be "Int{v|v>0}", and "let y = .." should be "let y:Int{..} = ..".
From what I can tell, the extra enter thing is now fixed(?)

--- other files already answered ---

varName.txt:
Fixed the parse error.
Valid.

varType.txt:
Valid.