% http://oberon07.com/EBNF.txt

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
length element set setlist designator deselem deslist deslist2 identlist idlist fieldlistsequence_list t_array_list
typedeclaration type arraytype lenlist fieldlist fieldlistsequence recordtype fldlist
pointertype variabledeclaration formaltype proceduretype fpsection idlist2 fpseclist formalparameters 
procedureheading
.

Terminals 
integer real
ident t_import t_semicolon t_is t_in t_moreeq t_more t_lesseq t_less
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
% module -> set : '$1'.
% module -> expression : '$1'.
% module -> constantdeclaration : '$1'.
% module -> explist : '$1'.
% module -> actualparameters : '$1'.
% module -> designator : '$1'.
% module -> identlist : '$1'.
% module -> fieldlist : '$1'.
% module -> fieldlistsequence : '$1'.
% module -> type : '$1'.
% module -> formaltype : '$1'.
% module -> fpsection : '$1'.
% module -> formalparameters : '$1'.
% module -> type : '$1'.
% module -> variabledeclaration : '$1'.
% module -> procedureheading : '$1'.




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
importlist -> t_import implist t_semicolon : {importlist, str_of('$1'), '$2'}.
implist -> import : ['$1'].
implist -> import t_comma implist : ['$1'] ++ '$3'.

% import  =  ident [":=" ident]. 
import -> ident : {import, str_of('$1'), {value_of('$1'), value_of('$1')}}.
import -> ident t_assign ident: {import, str_of('$1'), {value_of('$1'), value_of('$3')}}.

% DeclarationSequence = [CONST {ConstDeclaration ";"}] 
%  [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}] 
%  {ProcedureDeclaration ";"}.

% ConstantDeclaration  =  identdef "=" ConstExpression. 
constantdeclaration -> identdef t_equ constexpression : {constantdeclaration, '$1', '$3'}.

% identdef  =  ident ["*"]. 
identdef -> ident       : {identdef, str_of('$1'), value_of('$1')}.
identdef -> ident t_mul : {identdef, str_of('$1'), value_of('$1') ++ "*"}.

% ConstExpression  =  expression. 
constexpression -> expression : {constexpression, str_of('$1'), value_of('$1')}.

% expression  =  SimpleExpression [relation SimpleExpression]. 
expression -> simpleexpression : {expression, str_of('$1'), value_of('$1')}.
expression -> simpleexpression relation simpleexpression: {expression, str_of('$1'), {'$2', value_of('$1'), value_of('$3')}}.

% SimpleExpression  =  ["+"|"-"] term {AddOperator term}. 
simpleexpression -> term : {simpleexpression, str_of('$1'), value_of('$1')}.
simpleexpression -> t_plus term  : {simpleexpression, str_of('$1'), {plus, value_of('$2')}}.
simpleexpression -> t_minus term : {simpleexpression, str_of('$1'), {minus, value_of('$2')}}.
simpleexpression -> simpleexpression addoperator term : {simpleexpression, str_of('$1'), {'$2', value_of('$1'), value_of('$3')}}.

% term  =  factor {MulOperator factor}. 
term -> factor : {term, str_of('$1'), value_of('$1')}.
term -> term muloperator term: {term, str_of('$1'), {'$2', value_of('$1'), value_of('$3')}}.

% factor  =  number | CharConstant | string | NIL | set | 
%     designator [ActualParameters] | "(" expression ")" | "~" factor. 
factor -> number : {factor, str_of('$1'), '$1'}.
factor -> character : {factor, str_of('$1'), '$1'}.
factor -> string : {factor, str_of('$1'), '$1'}.
factor -> t_nil : {factor, str_of('$1'), '$1'}.
factor -> set : {factor, str_of('$1'), '$1'}.
factor -> designator : {factor, str_of('$1'), '$1'}.
factor -> designator actualparameters: {factor, str_of('$1'), {'$1', '$2'}}.
factor -> t_lpar expression t_rpar : {factor, str_of('$1'), '$2'}.
factor -> t_tilda factor : {factor, str_of('$1'), {t_tilda, '$2'}}.

% set  =  "{" [element {"," element}] "}". 
set -> t_lbrace t_rbrace : {set, str_of('$1'), []}.
set -> t_lbrace setlist t_rbrace : {set, str_of('$1'), '$2'}.
setlist -> element : ['$1'].
setlist -> element t_comma setlist : ['$1'] ++ '$3'.


% element  =  expression [".." expression]. 
element -> expression : {element, str_of('$1'), value_of('$1')}.
element -> expression t_ddot expression: {element, str_of('$1'), {t_ddot, value_of('$1'), value_of('$3')}}.

% designator  =  qualident {"." ident | "[" ExpList "]" | "(" qualident ")" | "^" }. 
designator -> deslist: {designator, str_of('$1'), value_of('$1')}.

deslist -> qualident : {deslist, str_of('$1'), [{qualident, str_of('$1'),value_of('$1')}]}.
deslist -> qualident deslist2: {deslist, str_of('$1'), [{qualident, str_of('$1'), value_of('$1')}]++value_of('$2')}.

deslist2 -> deselem : {deslist2, str_of('$1'), [value_of('$1')]}.
deslist2 -> deslist2 deselem : {deslist2, str_of('$1'), value_of('$1') ++ [value_of('$2')]}.

deselem -> t_dot ident : {deselem, str_of('$1'), {ident, str_of('$1'), value_of('$2')}}.
deselem -> t_lbrack explist t_rbrack : {deselem, str_of('$1'), {explist, str_of('$1'), value_of('$2')}}.
deselem -> t_lpar qualident t_rpar  : {deselem, str_of('$1'), {pars_qualident, str_of('$1'), '$2'}}.
deselem -> t_arrow : {deselem, str_of('$1'), t_arrow}. % {4, t_arrow}.


% ExpList  =  expression {"," expression}. 
explist -> exlist : {explist, str_of('$1'), value_of('$1')}.
exlist -> expression : {exlist, str_of('$1'), [value_of('$1')]}.
exlist -> expression t_comma exlist : {exlist, str_of('$1'),[value_of('$1')] ++ value_of('$3')}.


% ActualParameters  =  "(" [ExpList] ")". 
actualparameters -> t_lpar t_rpar : {actualparameters, str_of('$1'), []}.
actualparameters -> t_lpar explist t_rpar : {actualparameters, str_of('$1'), value_of('$2')}.

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

% StrucType = ArrayType | RecordType | PointerType | ProcedureType.
% type = qualident | StrucType.
% type  =  qualident | ArrayType | RecordType | PointerType | ProcedureType. 
type -> qualident : {type, str_of('$1'), '$1'}.
type -> arraytype : {type, str_of('$1'), '$1'}.
type -> recordtype : {type, str_of('$1'), '$1'}.
type -> proceduretype : {type, str_of('$1'), '$1'}.
type -> pointertype : {type, str_of('$1'), '$1'}.

% qualident  =  [ident "."] ident. 
qualident -> ident : {qualident, str_of('$1'), value_of('$1')}.
qualident -> ident t_dot ident : {qualident, str_of('$1'), value_of('$1')++"."++value_of('$3')}.

% ArrayType  =  ARRAY length {"," length} OF type. 
arraytype -> t_array lenlist t_of type : {arraytype, str_of('$1'), {'$4' ,'$2'}}.
lenlist -> length : ['$1'].
lenlist -> length t_comma lenlist : ['$1'] ++ '$3'.


% length  =  ConstExpression. 
length -> constexpression : '$1'.

% RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
recordtype -> t_record t_end : {recordtype, str_of('$1'), {nil, []}}.
recordtype -> t_record fieldlistsequence t_end : {recordtype, str_of('$1'), {nil, '$2'}}.

% BaseType  =  qualident. 
basetype -> qualident : : {basetype, str_of('$1'), value_of('$1')}.

% FieldListSequence  =  FieldList {";" FieldList}. 
fieldlistsequence -> fieldlistsequence_list : {fieldlistsequence, str_of('$1'), value_of('$1')}.

fieldlistsequence_list -> fieldlist : {fieldlistsequence_list, str_of('$1'), ['$1']}.
fieldlistsequence_list -> fieldlist t_semicolon : {fieldlistsequence_list, str_of('$1'), ['$1']}.
fieldlistsequence_list -> fieldlist t_semicolon fieldlistsequence_list : {fieldlistsequence_list, str_of('$1'), ['$1'] ++ value_of('$3')}.
% fieldlistsequence_list -> fieldlist t_semicolon fieldlistsequence_list t_semicolon : {fieldlistsequence_list, str_of('$1'), ['$1'] ++ value_of('$3')}.


% FieldList = IdentList ":" type.
fieldlist -> identlist t_colon type : {fieldlist, str_of('$1'), {value_of('$1'), value_of('$3')}}.

% IdentList  =  identdef {"," identdef}. 
identlist -> idlist : {identlist, str_of('$1'), value_of('$1')}.
idlist -> identdef : {idlist, str_of('$1'), ['$1']}.
idlist -> identdef t_comma idlist: {idlist, str_of('$1'), ['$1'] ++ value_of('$3')}.

% PointerType  =  POINTER TO type. 
pointertype -> t_pointer t_to type : {pointertype, str_of('$1'), '$3'}.

% ProcedureType  =  PROCEDURE [FormalParameters]. 
proceduretype -> t_procedure : {proceduretype, str_of('$1'), []}.
proceduretype -> t_procedure formalparameters: {proceduretype, str_of('$1'), '$2'}.

% VariableDeclaration  =  IdentList ":" type. 
variabledeclaration -> identlist t_colon type : {variabledeclaration, str_of('$1'), {'$1', '$3'}}.

% ProcedureDeclaration  =  ProcedureHeading ";" ProcedureBody ident. 

% ProcedureHeading  =  PROCEDURE ["*"] identdef [FormalParameters]. 
procedureheading -> t_procedure identdef : {procedureheading, str_of('$1'), {nil, '$2', []}}.
procedureheading -> t_procedure t_mul identdef : {procedureheading, str_of('$1'), {ptr, '$3', []}}.
procedureheading -> t_procedure t_mul identdef formalparameters : {procedureheading, str_of('$1'), {ptr, '$3', '$4'}}.
procedureheading -> t_procedure identdef formalparameters : {procedureheading, str_of('$1'), {nil, '$2', '$3'}}.

% FormalParameters  =  "(" [FPSection {";" FPSection}] ")" [":" qualident]. 
formalparameters -> t_lpar t_rpar : {formalparameters, str_of('$1'), {[], nil}}.
formalparameters -> t_lpar t_rpar t_colon qualident : {formalparameters, str_of('$1'), {[], '$4'}}.
formalparameters -> t_lpar fpseclist t_rpar : {formalparameters, str_of('$1'), {'$2', nil}}.
formalparameters -> t_lpar fpseclist t_rpar t_colon qualident : {formalparameters, str_of('$1'), {'$2', '$5'}}.

fpseclist -> fpsection : ['$1'].
fpseclist -> fpsection t_semicolon fpseclist : ['$1'] ++ '$3'.

% FPSection  =  [VAR] ident {"," ident} ":" FormalType. 
fpsection -> idlist2 t_colon formaltype : {fpsection, str_of('$1'), {not_var, value_of('$1'), '$3'}}.
fpsection -> t_var idlist2 t_colon formaltype : {fpsection, str_of('$1'), {var, value_of('$2'), '$4'}}.

idlist2 -> ident : {idlist2, str_of('$1'), ['$1']}.
idlist2 -> ident t_comma idlist2 : {idlist2, str_of('$1'), ['$1'] ++ value_of('$3')}.

% FormalType  =  {ARRAY OF} qualident. 
formaltype -> qualident : {formaltype, str_of('$1'), '$1'}.
formaltype -> t_array_list qualident : {formaltype, str_of('$1'), {array_of, value_of('$1'), '$2'}}.

t_array_list -> t_array t_of : {t_array_list, str_of('$1'), 1}.
t_array_list -> t_array_list t_array t_of : {t_array_list, str_of('$1'), value_of('$1') + 1}.


% ProcedureBody  =  DeclarationSequence [BEGIN StatementSequence] END. 


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