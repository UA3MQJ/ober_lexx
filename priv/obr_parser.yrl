Nonterminals module number exprlist expr simpleexpr mulop addop factor term
relation
.

Terminals integer real t_lpar t_rpar character string t_comma t_nil
t_tilda t_equ t_sharp t_less t_lesseq t_more t_moreeq t_in t_is
t_plus t_minus t_or 
t_mul t_divide t_div t_mod t_and.

Rootsymbol module.

module -> expr : '$1'.

% Expr = SimpleExpr [Relation SimpleExpr].
expr -> simpleexpr relation simpleexpr: {'$2', '$1', '$3'}.
expr -> simpleexpr : '$1'.

% SimpleExpr = ["+" | "-"] Term {AddOp Term}. 
simpleexpr -> t_minus term addop term : {'$3', {t_minus, '$2'}, '$4'}.
simpleexpr -> t_plus term addop term : {'$3', {t_plus, '$2'}, '$4'}.
simpleexpr -> t_minus term : {t_minus, '$1'}.
simpleexpr -> t_plus term : {t_plus, '$1'}.
simpleexpr -> term : '$1'.

% Term = Factor {MulOp Factor}.
term -> factor mulop factor : {'$2', '$1', '$3'}.
factor -> t_lpar factor t_rpar : '$2'.
term -> factor : '$1'.

factor -> number : '$1'.
factor -> character : '$1'.
factor -> string : '$1'.
factor -> t_nil : '$1'.
factor -> t_lpar expr t_rpar : '$2'.
factor -> t_tilda factor : {t_tilda, '$2'}.

% Relation = "=" | "#" | "<" | "<=" | ">" | ">=" | "IN" | "IS".
relation -> t_equ : '$1'.
relation -> t_sharp : '$1'.
relation -> t_less : '$1'.
relation -> t_lesseq : '$1'.
relation -> t_more : '$1'.
relation -> t_moreeq : '$1'.
relation -> t_in : '$1'.
relation -> t_is : '$1'.

% AddOp = "+" | "-" | "OR".
addop -> t_plus : '$1'.
addop -> t_minus : '$1'.
addop -> t_or : '$1'.

% MulOp = "*" | "/" | "DIV" | "MOD" | "&".
mulop -> t_mul : '$1'.
mulop -> t_divide : '$1'.
mulop -> t_div : '$1'.
mulop -> t_mod : '$1'.
mulop -> t_and : '$1'.

number -> integer : '$1'.
number -> real : '$1'.

Erlang code.