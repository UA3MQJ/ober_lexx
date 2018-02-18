% Для описания синтаксиса Оберона-2 используются Расширенные Бэкуса-Наура Формы (РБНФ). 
% Варианты разделяются знаком |. 
% Квадратные скобки [ и ] означают необязательность записанного внутри них выражения, 
% а фигурные скобки { и } означают его повторение (возможно 0 раз). 
% Нетерминальные символы начинаются с заглавной буквы (например, Оператор). 
% Терминальные символы или начинаются малой буквой (например, идент), 
% или записываются целиком заглавными буквами (например, BEGIN), или заключаются в кавычки (например, ":=").

Nonterminals 
module number muloperator addoperator relation import implist importlist qualident identdef basetype
factor term simpleexpression expression constexpression constantdeclaration explist exlist actualparameters
length element set setlist designator deselem deslist deslist2 identlist idlist
typedeclaration type arraytype lenlist fieldlist fieldlistsequence recordtype fldlist
pointertype variabledeclaration formaltype proceduretype fpsection idlist2 fpseclist formalparameters 
procedureheading forwarddeclaration
.

Terminals 
ident integer real t_import t_semicolon t_is t_in t_moreeq t_more t_lesseq t_less
t_sharp t_equ t_or t_minus t_plus t_and t_mod t_div t_divide t_mul t_assign t_comma t_dot
character string t_nil t_tilda t_lpar t_rpar t_ddot t_lbrace t_rbrace t_arrow t_lbrack t_rbrack
t_array t_of t_end t_record t_colon t_pointer t_to t_var t_procedure
.

Rootsymbol module.

% Rigth      100 ident.
% Rigth      200 qualident.

% module -> number : '$1'.
% module -> importlist : '$1'.
% module -> qualident : '$1'.
% module -> identdef : '$1'.
% module -> constantdeclaration : '$1'.
% module -> explist : '$1'.
% module -> actualparameters : '$1'.
% module -> set : '$1'.
% module -> designator : '$1'.
% module -> identlist : '$1'.
% module -> expression : '$1'.
% module -> type : '$1'.
% module -> variabledeclaration : '$1'.
% module -> formalparameters : '$1'.
% module -> procedureheading : '$1'.
module -> forwarddeclaration : '$1'.




% то, что уже есть благодаря лексеру
% ident  =  letter {letter | digit}. - ident уже идет из лексера
% letter =  "A" .. "Z" | "a" .. "z". 
% digit  =  "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9". 
% integer  =  digit {digit} | digit {hexDigit} "H". 
% real  =  digit {digit} "." {digit} [ScaleFactor]. 
% hexDigit  =  digit | "A" | "B" | "C" | "D" | "E" | "F". 
% CharConstant  =  '"' character '"' | digit {hexDigit} "X". 
% string  =  '"' {character} '"'. 
% ScaleFactor  =  ("E" | "D") ["+" | "-"] digit {digit}. 
% -----------------------------------------------------------------------------------

% number  =  integer | real. 
number -> integer : '$1'.
number -> real : '$1'.

% module  =  MODULE ident ";"  [ImportList] DeclarationSequence 
%     [BEGIN StatementSequence] END ident ".". 

% ImportList  =  IMPORT import {"," import} ";". 
importlist -> t_import implist t_semicolon : {importlist, '$2'}.
implist -> import : ['$1'].
implist -> import t_comma implist : ['$1'] ++ '$3'.

% import  =  ident [":=" ident]. 
import -> ident : {import, '$1', '$1'}.
import -> ident t_assign ident: {import, '$1', '$3'}.

% DeclarationSequence  =  {CONST {ConstantDeclaration ";"} | 
%     TYPE {TypeDeclaration ";"} | VAR {VariableDeclaration ";"}} 
%     {ProcedureDeclaration ";" | ForwardDeclaration ";"}. 

% ConstantDeclaration  =  identdef "=" ConstExpression. 
constantdeclaration -> identdef t_equ constexpression : {constantdeclaration, '$1', '$3'}.

% identdef  =  ident ["*"]. 
identdef -> ident       : {identdef, str_of('$1'), value_of('$1')}.
identdef -> ident t_mul : {identdef, str_of('$1'), value_of('$1') ++ "*"}.

% ConstExpression  =  expression. 
constexpression -> expression : '$1'.

% expression  =  SimpleExpression [relation SimpleExpression]. 
expression -> simpleexpression : '$1'.
expression -> simpleexpression relation simpleexpression: {relation, '$1', '$3'}.

% SimpleExpression  =  ["+"|"-"] term {AddOperator term}. 
simpleexpression -> term : '$1'.
simpleexpression -> t_plus term  : {t_plus, '$2'}.
simpleexpression -> t_minus term : {t_minus, '$2'}.
simpleexpression -> term addoperator simpleexpression : {'$2', '$1', '$3'}.
% simpleexpression -> t_plus  term addoperator term : {'$3', {t_plus, '$2'}, '$4'}.
% simpleexpression -> t_minus term addoperator term : {'$3', {t_minus, '$2'}, '$4'}.

% term  =  factor {MulOperator factor}. 
term -> factor : '$1'.
term -> factor muloperator term: {'$2', '$1', '$3'}.

% factor  =  number | CharConstant | string | NIL | set | 
%     designator [ActualParameters] | "(" expression ")" | "~" factor. 
factor -> number : '$1'.
factor -> character : '$1'.
factor -> string : '$1'.
factor -> t_nil : '$1'.
factor -> set : '$1'.
factor -> designator : '$1'.
factor -> designator actualparameters: {'$1', '$2'}.
factor -> t_lpar expression t_rpar : '$2'.
factor -> t_tilda factor : {t_tilda, '$2'}.

% set  =  "{" [element {"," element}] "}". 
set -> t_lbrace t_rbrace : {set, []}.
set -> t_lbrace setlist t_rbrace : {set, '$2'}.
setlist -> element : ['$1'].
setlist -> element t_comma setlist : ['$1'] ++ '$3'.


% element  =  expression [".." expression]. 
element -> expression : '$1'.
element -> expression t_ddot expression: {t_ddot, '$1', '$3'}.

% designator  =  qualident {"." ident | "[" ExpList "]" | "(" qualident ")" | "^" }. 
designator -> deslist: {designator, '$1'}.

deslist -> qualident : '$1'.
deslist -> qualident deslist2: {'$1', '$2'}.

deslist2 -> deselem : '$1'.
deslist2 -> deslist2 deselem : {'$1', '$2'}.

deselem -> t_dot ident : {1, '$2'}.
deselem -> t_lbrack exlist t_rbrack : {2, '$2'}.
deselem -> t_lpar qualident t_rpar  : {3, '$2'}.
deselem -> t_arrow : {4, t_arrow}.


% ExpList  =  expression {"," expression}. 
explist -> exlist : '$1'.
exlist -> expression : ['$1'].
exlist -> expression t_comma exlist : ['$1'] ++ '$3'.


% ActualParameters  =  "(" [ExpList] ")". 
actualparameters -> t_lpar t_rpar : {}.
actualparameters -> t_lpar explist t_rpar : {'$2'}.

% MulOperator  =  "*" | "/" | DIV | MOD | "&". 
muloperator -> t_mul : '$1'.
muloperator -> t_divide : '$1'.
muloperator -> t_div : '$1'.
muloperator -> t_mod : '$1'.
muloperator -> t_and : '$1'.

% AddOperator  =  "+" | "-" | OR. 
addoperator -> t_plus : '$1'.
addoperator -> t_minus : '$1'.
addoperator -> t_or : '$1'.

% relation  =  "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS. 
relation -> t_equ : '$1'.
relation -> t_sharp : '$1'.
relation -> t_less : '$1'.
relation -> t_lesseq : '$1'.
relation -> t_more : '$1'.
relation -> t_moreeq : '$1'.
relation -> t_in : '$1'.
relation -> t_is : '$1'.

% TypeDeclaration  =  identdef "=" type. 
typedeclaration -> identdef t_equ type : {typedeclaration, '$1', '$3'}.

% type  =  qualident | ArrayType | RecordType | PointerType | ProcedureType. 
type -> qualident : {type, '$1'}.
type -> arraytype : {type, '$1'}.
type -> recordtype : {type, '$1'}.
type -> pointertype : {type, '$1'}.
type -> proceduretype : {type, '$1'}.

% qualident  =  [ident "."] ident. 
qualident -> ident : '$1'.
qualident -> ident t_dot ident : {t_dot, '$1', '$3'}.

% ArrayType  =  ARRAY length {"," length} OF type. 
arraytype -> t_array lenlist t_of type : {arraytype, '$2', t_of, '$4'}.
lenlist -> length : ['$1'].
lenlist -> length t_comma lenlist : ['$1'] ++ '$3'.


% length  =  ConstExpression. 
length -> constexpression : '$1'.

% RecordType  =  RECORD ["(" BaseType ")"] FieldListSequence END. 
recordtype -> t_record fieldlistsequence t_end : {recordtype, '$2'}.

% BaseType  =  qualident. 
basetype -> qualident : : {basetype, str_of('$1'), value_of('$1')}.

% FieldListSequence  =  FieldList {";" FieldList}. 
fieldlistsequence -> fldlist : {fieldlistsequence, '$1'}.

fldlist -> fieldlist : ['$1'].
fldlist -> fieldlist t_semicolon fldlist : ['$1'] ++ '$3'.

% FieldList  =  [IdentList ":" type]. 
fieldlist -> identlist t_colon type : {fieldlist, '$1', '$3'}.

% IdentList  =  identdef {"," identdef}. 
identlist -> idlist : '$1'.
idlist -> identdef : ['$1'].
idlist -> identdef t_comma idlist : ['$1'] ++ '$3'.

% PointerType  =  POINTER TO type. 
pointertype -> t_pointer t_to type : {t_pointer, '$3'}.

% ProcedureType  =  PROCEDURE [FormalParameters]. 
proceduretype -> t_procedure : {t_procedure, []}.
proceduretype -> t_procedure formalparameters: {t_procedure, '$2'}.

% VariableDeclaration  =  IdentList ":" type. 
variabledeclaration -> identlist t_colon type : {variabledeclaration, '$1', '$3'}.

% ProcedureDeclaration  =  ProcedureHeading ";" ProcedureBody ident. 

% ProcedureHeading  =  PROCEDURE ["*"] identdef [FormalParameters]. 
procedureheading -> t_procedure identdef : {t_procedure, '$2'}.
procedureheading -> t_procedure t_mul identdef : {t_procedure, t_mul, '$3'}.
procedureheading -> t_procedure t_mul identdef formalparameters : {t_procedure, t_mul, '$3', '$4'}.
procedureheading -> t_procedure identdef formalparameters : {t_procedure, '$2', '$3'}.

% FormalParameters  =  "(" [FPSection {";" FPSection}] ")" [":" qualident]. 
formalparameters -> t_lpar t_rpar : {formalparameters, []}.
formalparameters -> t_lpar t_rpar t_colon qualident : {formalparameters, [], '$4'}.
formalparameters -> t_lpar fpseclist t_rpar : {formalparameters, '$2'}.
formalparameters -> t_lpar fpseclist t_rpar t_colon qualident : {formalparameters, '$2', '$5'}.

fpseclist -> fpsection : ['$1'].
fpseclist -> fpsection t_semicolon fpseclist : ['$1'] ++ '$3'.

% FPSection  =  [VAR] ident {"," ident} ":" FormalType. 
fpsection -> idlist2 t_colon formaltype : {fpsection, '$1', '$3'}.
fpsection -> t_var idlist2 t_colon formaltype : {fpsection, t_var, '$2', '$4'}.

idlist2 -> identdef : ['$1'].
idlist2 -> identdef t_comma idlist2 : ['$1'] ++ '$3'.

% FormalType  =  {ARRAY OF} (qualident | ProcedureType). 
formaltype -> qualident     : {formaltype, '$1'}.
formaltype -> proceduretype : {formaltype, '$1'}.
formaltype -> t_array t_of qualident     : {formaltype, t_array, '$1'}.
formaltype -> t_array t_of proceduretype : {formaltype, t_array, '$1'}.

% ProcedureBody  =  DeclarationSequence [BEGIN StatementSequence] END. 


% ForwardDeclaration  =  PROCEDURE "^" ident ["*"] [FormalParameters]. 
forwarddeclaration -> t_procedure t_arrow ident : {forwarddeclaration, '$3'}.
forwarddeclaration -> t_procedure t_arrow ident t_mul: {forwarddeclaration, '$3', t_mul}.
forwarddeclaration -> t_procedure t_arrow ident formalparameters: {forwarddeclaration, '$3', '$4'}.
forwarddeclaration -> t_procedure t_arrow ident t_mul formalparameters: {forwarddeclaration, '$3', t_mul, '$5'}.

% StatementSequence  =  statement {";" statement}. 
% statement  =  [assignment | ProcedureCall | 
%     IfStatement | CaseStatement | WhileStatement | RepeatStatement | 
%     LoopStatement | WithStatement | EXIT | RETURN [expression] ]. 
% assignment  =  designator ":=" expression. 
% ProcedureCall  =  designator [ActualParameters]. 
% IfStatement  =  IF expression THEN StatementSequence 
%     {ELSIF expression THEN StatementSequence} 
%     [ELSE StatementSequence] END. 
% CaseStatement  =  CASE expression OF case {"|" case} 
%     [ELSE StatementSequence] END. 
% case  =  [CaseLabelList ":" StatementSequence]. 
% CaseLabelList  =  CaseLabels {"," CaseLabels}. 
% CaseLabels  =  ConstExpression [".." ConstExpression]. 
% WhileStatement  =  WHILE expression DO StatementSequence END. 
% RepeatStatement  =  REPEAT StatementSequence UNTIL expression. 
% LoopStatement  =  LOOP StatementSequence END. 
% WithStatement  =  WITH qualident ":" qualident DO StatementSequence END.


Erlang code.

list_tail({_, List}) -> List.

str_of(Token) ->
    element(2, Token).

value_of(Token) ->
    element(3, Token).