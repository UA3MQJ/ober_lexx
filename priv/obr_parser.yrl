Nonterminals 
module identlist exprlist element elementlist set identdef number simpleexpr mulop addop factor term
relation constexpr expr caselabels caselabelslist ccase
.

Terminals 
ident integer real t_lpar t_rpar character string t_comma t_nil
t_tilda t_equ t_sharp t_less t_lesseq t_more t_moreeq t_in t_is
t_ddot t_lbrace t_rbrace
t_plus t_minus t_or 
t_mul t_divide t_div t_mod t_and.

Rootsymbol module.

% module -> expr : '$1'.
% module -> identdef : '$1'.
% module -> identlist : '$1'.
% module -> exprlist : '$1'.
% module -> element : '$1'.
% module -> set : '$1'.
% module -> constexpr : '$1'.
% module -> caselabels : '$1'.
module -> ccase : '$1'.

% !!!!
% Case = [CaseLabels {"," CaseLabels} ":" StatementSeq]. 
% ccase -> caselabelslist t_colon ...
% caselabelslist -> caselabels                       : {caselabelslist, ['$1']}.
% caselabelslist -> caselabels t_comma caselabelslist: {caselabelslist, ['$1'] ++ list_tail('$3')}.

% CaseLabels = ConstExpr [".." ConstExpr].
caselabels -> constexpr                  : {caselabels, {'$1'}}.
caselabels -> constexpr t_ddot constexpr : {caselabels, {'$1', '$3'}}.

% !!!!!
% Guard = Qualident ":" Qualident.

% ConstExpr = Expr.
constexpr -> expr : '$1'.

% Expr = SimpleExpr [Relation SimpleExpr].
expr -> simpleexpr relation simpleexpr : {'$2', '$1', '$3'}.
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

%   = Designator ["(" [ExprList] ")"] !!!!
factor -> number : '$1'.
factor -> character : '$1'.
factor -> string : '$1'.
factor -> t_nil : '$1'.
factor -> t_lpar expr t_rpar : '$2'.
factor -> t_tilda factor : {t_tilda, '$2'}.


% Set = "{" [Element {"," Element}] "}". 
set -> t_lbrace t_rbrace : {set, []}.
set -> t_lbrace elementlist t_rbrace : {set, ['$2']}.

elementlist -> element : {identlist, ['$1']}.
elementlist -> element t_comma elementlist : {elementlistt, ['$1'] ++ list_tail('$3')}.

% Element = Expr [".." Expr].
element -> expr            : {element, {'$1'}}.
element -> expr t_ddot expr: {element, {'$1', '$3'}}.

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

% !!!!!
% Designator = Qualident { IF(isDesignatorPart()) ("." ident | "[" ExprList "]" | "^" | "(" Qualident ")") }.

% ExprList = Expr {"," Expr}.
exprlist -> expr : {exprlist, ['$1']}.
exprlist -> expr t_comma exprlist : {exprlist, ['$1'] ++ list_tail('$3')}.

% IdentList = IdentDef {"," IdentDef}.
identlist -> identdef : {identlist, ['$1']}.
identlist -> identdef t_comma identlist : {identlist, ['$1'] ++ list_tail('$3')}.

% !!!!!
% Qualident = [IF(isModule()) ident "."] ident.

% IdentDef = ident ["*" | "-"].
identdef -> ident : {ident, '', value_of('$1')}.
identdef -> ident t_mul : {ident, '*', value_of('$1')}.
identdef -> ident t_minus: {ident, '-', value_of('$1')}.

% number = integer | real.
number -> integer : '$1'.
number -> real : '$1'.

Erlang code.

list_tail({_, List}) -> List.

value_of(Token) ->
    element(3, Token).