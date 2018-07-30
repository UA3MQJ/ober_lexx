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
typedeclaration type structype arraytype lenlist fieldlist fieldlistsequence recordtype fldlist
pointertype variabledeclaration formaltype proceduretype fpsection idlist2 fpseclist formalparameters 
procedureheading label labelrange cllist caselabellist assignment
.

Terminals 
integer real
ident t_import t_semicolon t_is t_in t_moreeq t_more t_lesseq t_less
t_sharp t_equ t_or t_minus t_plus t_and t_mod t_div t_divide t_mul t_assign t_comma t_dot
character string t_nil t_true t_false t_tilda t_lpar t_rpar t_ddot t_lbrace t_rbrace t_arrow t_lbrack t_rbrack
t_array t_of t_end t_record t_colon t_pointer t_to t_var t_procedure
.

Rootsymbol module.

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
% module -> length : '$1'.
% module -> typedeclaration : '$1'.
% module -> constantdeclaration : '$1'.
% module -> label : '$1'.
% module -> labelrange : '$1'.
% module -> caselabellist : '$1'.
module -> assignment : '$1'.







% то, что уже есть благодаря лексеру
% +ident = letter {letter | digit}.
% +letter = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z".
% +digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9".
% +integer = digit {digit} | digit {hexDigit} "H".
% +real = digit {digit} "." {digit} [ScaleFactor].
% +ScaleFactor = "E" ["+" | "-"] digit {digit}.
% +hexDigit = digit | "A" | "B" | "C" | "D" | "E" | "F".
% +string = """ {character} """ | digit {hexDigit} "X".
% -----------------------------------------------------------------------------------

% +number = integer | real.
number -> integer : '$1'.
number -> real : '$1'.

% TODO
% module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .

% +ImportList = IMPORT import {"," import} ";".
importlist -> t_import implist t_semicolon : {importlist, str_of('$1'), '$2'}.
implist -> import : ['$1'].
implist -> import t_comma implist : ['$1'] ++ '$3'.
 
% +import = ident [":=" ident].
import -> ident : {import, str_of('$1'), {value_of('$1'), value_of('$1')}}.
import -> ident t_assign ident: {import, str_of('$1'), {value_of('$1'), value_of('$3')}}.

% TODO
% DeclarationSequence = [CONST {ConstDeclaration ";"}] [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}] {ProcedureDeclaration ";"}.

% +ConstDeclaration = identdef "=" ConstExpression.
constantdeclaration -> identdef t_equ constexpression : {constantdeclaration, '$1', '$3'}.

% +ConstExpression = expression.
constexpression -> expression : {constexpression, str_of('$1'), value_of('$1')}.

% +TypeDeclaration = identdef "=" StrucType.
typedeclaration -> identdef t_equ type : {typedeclaration, str_of('$1'), {'$1', '$3'}}.

% +StrucType = ArrayType | RecordType | PointerType | ProcedureType.
structype -> arraytype : {structype, str_of('$1'), '$1'}.
structype -> recordtype : {structype, str_of('$1'), '$1'}.
structype -> proceduretype : {structype, str_of('$1'), '$1'}.
structype -> pointertype : {structype, str_of('$1'), '$1'}.

% +ArrayType = ARRAY length {"," length} OF type.
arraytype -> t_array lenlist t_of type : {arraytype, str_of('$1'), {'$4' ,'$2'}}.
lenlist -> length : ['$1'].
lenlist -> length t_comma lenlist : ['$1'] ++ '$3'.

% +length = ConstExpression.
length -> constexpression : {length, str_of('$1'), '$1'}.

% +RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
recordtype -> t_record t_end : {recordtype, str_of('$1'), {nil, []}}.
recordtype -> t_record fieldlistsequence t_end : {recordtype, str_of('$1'), {nil, '$2'}}.
recordtype -> t_record t_lpar basetype t_rpar t_end : {recordtype, str_of('$1'), {'$3', []}}.
recordtype -> t_record t_lpar basetype t_rpar fieldlistsequence t_end : {recordtype, str_of('$1'), {'$3', '$5'}}.

% +BaseType = qualident.
basetype -> qualident : {basetype, str_of('$1'), value_of('$1')}.

% +FieldListSequence = FieldList {";" FieldList}. 
fieldlistsequence -> fieldlistsequence_list : {fieldlistsequence, str_of('$1'), value_of('$1')}.

fieldlistsequence_list -> fieldlist : {fieldlistsequence_list, str_of('$1'), ['$1']}.
fieldlistsequence_list -> fieldlist t_semicolon : {fieldlistsequence_list, str_of('$1'), ['$1']}.
fieldlistsequence_list -> fieldlist t_semicolon fieldlistsequence_list : {fieldlistsequence_list, str_of('$1'), ['$1'] ++ value_of('$3')}.
% fieldlistsequence_list -> fieldlist t_semicolon fieldlistsequence_list t_semicolon : {fieldlistsequence_list, str_of('$1'), ['$1'] ++ value_of('$3')}.

% +FieldList = IdentList ":" type.
fieldlist -> identlist t_colon type : {fieldlist, str_of('$1'), {value_of('$1'), value_of('$3')}}.

% +IdentList = identdef {"," identdef}.
identlist -> idlist : {identlist, str_of('$1'), value_of('$1')}.
idlist -> identdef : {idlist, str_of('$1'), ['$1']}.
idlist -> identdef t_comma idlist: {idlist, str_of('$1'), ['$1'] ++ value_of('$3')}.

% +PointerType = POINTER TO type.
pointertype -> t_pointer t_to type : {pointertype, str_of('$1'), '$3'}.

% +ProcedureType = PROCEDURE [FormalParameters].
proceduretype -> t_procedure : {proceduretype, str_of('$1'), []}.
proceduretype -> t_procedure formalparameters: {proceduretype, str_of('$1'), '$2'}.

% +FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
formalparameters -> t_lpar t_rpar : {formalparameters, str_of('$1'), {[], nil}}.
formalparameters -> t_lpar t_rpar t_colon qualident : {formalparameters, str_of('$1'), {[], '$4'}}.
formalparameters -> t_lpar fpseclist t_rpar : {formalparameters, str_of('$1'), {'$2', nil}}.
formalparameters -> t_lpar fpseclist t_rpar t_colon qualident : {formalparameters, str_of('$1'), {'$2', '$5'}}.

fpseclist -> fpsection : ['$1'].
fpseclist -> fpsection t_semicolon fpseclist : ['$1'] ++ '$3'.

% +FPSection = [VAR] ident {"," ident} ":" FormalType.
fpsection -> idlist2 t_colon formaltype : {fpsection, str_of('$1'), {not_var, value_of('$1'), '$3'}}.
fpsection -> t_var idlist2 t_colon formaltype : {fpsection, str_of('$1'), {var, value_of('$2'), '$4'}}.

idlist2 -> ident : {idlist2, str_of('$1'), ['$1']}.
idlist2 -> ident t_comma idlist2 : {idlist2, str_of('$1'), ['$1'] ++ value_of('$3')}.

% +FormalType = {ARRAY OF} qualident.
formaltype -> qualident : {formaltype, str_of('$1'), '$1'}.
formaltype -> t_array_list qualident : {formaltype, str_of('$1'), {array_of, value_of('$1'), '$2'}}.

t_array_list -> t_array t_of : {t_array_list, str_of('$1'), 1}.
t_array_list -> t_array_list t_array t_of : {t_array_list, str_of('$1'), value_of('$1') + 1}.

% +qualident = [ident "."] ident.
qualident -> ident : {qualident, str_of('$1'), value_of('$1')}.
qualident -> ident t_dot ident : {qualident, str_of('$1'), value_of('$1')++"."++value_of('$3')}.

% +identdef = ident ["*"].
identdef -> ident       : {identdef, str_of('$1'), value_of('$1')}.
identdef -> ident t_mul : {identdef, str_of('$1'), value_of('$1') ++ "*"}.

% +VariableDeclaration = IdentList ":" type. 
variabledeclaration -> identlist t_colon type : {variabledeclaration, str_of('$1'), {'$1', '$3'}}.

% +type = qualident | StrucType.
type -> qualident : {type, str_of('$1'), '$1'}.
type -> structype : {type, str_of('$1'), '$1'}.

% TODO
% ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.

% +ProcedureHeading = PROCEDURE identdef [FormalParameters].
procedureheading -> t_procedure identdef : {procedureheading, str_of('$1'), {'$2', []}}.
procedureheading -> t_procedure identdef formalparameters : {procedureheading, str_of('$1'), {'$2', '$3'}}.

% TODO
% ProcedureBody = DeclarationSequence [BEGIN StatementSequence] [RETURN expression] END.

% +expression = SimpleExpression [relation SimpleExpression].
expression -> simpleexpression : {expression, str_of('$1'), value_of('$1')}.
expression -> simpleexpression relation simpleexpression: {expression, str_of('$1'), {'$2', value_of('$1'), value_of('$3')}}.

% +relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
relation -> t_equ : '$1'.
relation -> t_sharp : '$1'.
relation -> t_less : '$1'.
relation -> t_lesseq : '$1'.
relation -> t_more : '$1'.
relation -> t_moreeq : '$1'.
relation -> t_in : '$1'.
relation -> t_is : '$1'.

% +SimpleExpression = ["+" | "-"] term {AddOperator term}.
simpleexpression -> term : {simpleexpression, str_of('$1'), value_of('$1')}.
simpleexpression -> t_plus term  : {simpleexpression, str_of('$1'), {plus, value_of('$2')}}.
simpleexpression -> t_minus term : {simpleexpression, str_of('$1'), {minus, value_of('$2')}}.
simpleexpression -> simpleexpression addoperator term : {simpleexpression, str_of('$1'), {'$2', value_of('$1'), value_of('$3')}}.

% +AddOperator = "+" | "-" | OR.
addoperator -> t_plus : '$1'.
addoperator -> t_minus : '$1'.
addoperator -> t_or : '$1'.

% +term = factor {MulOperator factor}.
term -> factor : {term, str_of('$1'), value_of('$1')}.
term -> term muloperator term: {term, str_of('$1'), {'$2', value_of('$1'), value_of('$3')}}.

% +MulOperator = "*" | "/" | DIV | MOD | "&".
muloperator -> t_mul : '$1'.
muloperator -> t_divide : '$1'.
muloperator -> t_div : '$1'.
muloperator -> t_mod : '$1'.
muloperator -> t_and : '$1'.

% +factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
factor -> number : {factor, str_of('$1'), '$1'}.
factor -> string : {factor, str_of('$1'), '$1'}.
factor -> t_nil : {factor, str_of('$1'), '$1'}.
factor -> t_true : {factor, str_of('$1'), '$1'}.
factor -> t_false : {factor, str_of('$1'), '$1'}.
factor -> set : {factor, str_of('$1'), '$1'}.
factor -> designator : {factor, str_of('$1'), '$1'}.
factor -> designator actualparameters: {factor, str_of('$1'), {'$1', '$2'}}.
factor -> t_lpar expression t_rpar : {factor, str_of('$1'), '$2'}.
factor -> t_tilda factor : {factor, str_of('$1'), {t_tilda, '$2'}}.

% +designator = qualident {selector}.
% +selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
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

% +set = "{" [element {"," element}] "}".
set -> t_lbrace t_rbrace : {set, str_of('$1'), []}.
set -> t_lbrace setlist t_rbrace : {set, str_of('$1'), '$2'}.
setlist -> element : ['$1'].
setlist -> element t_comma setlist : ['$1'] ++ '$3'.

% +element = expression [".." expression].
element -> expression : {element, str_of('$1'), value_of('$1')}.
element -> expression t_ddot expression: {element, str_of('$1'), {t_ddot, value_of('$1'), value_of('$3')}}.

% +ExpList = expression {"," expression}.
explist -> exlist : {explist, str_of('$1'), value_of('$1')}.
exlist -> expression : {exlist, str_of('$1'), [value_of('$1')]}.
exlist -> expression t_comma exlist : {exlist, str_of('$1'),[value_of('$1')] ++ value_of('$3')}.

% +ActualParameters = "(" [ExpList] ")" .
actualparameters -> t_lpar t_rpar : {actualparameters, str_of('$1'), []}.
actualparameters -> t_lpar explist t_rpar : {actualparameters, str_of('$1'), value_of('$2')}.

% TODO
% statement = [assignment | ProcedureCall | IfStatement | CaseStatement |     WhileStatement | RepeatStatement | ForStatement].

% assignment = designator ":=" expression.
assignment -> designator t_assign expression : {assignment, str_of('$1'), {'$1', '$3'}}.

% TODO
% ProcedureCall = designator [ActualParameters].

% TODO
% StatementSequence = statement {";" statement}.

% TODO
% IfStatement = IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence} [ELSE StatementSequence] END.

% TODO
% CaseStatement = CASE expression OF case {"|" case} END.

% TODO
% case = [CaseLabelList ":" StatementSequence].

% CaseLabelList = LabelRange {"," LabelRange}.
caselabellist -> cllist : {caselabellist, str_of('$1'), value_of('$1')}.
cllist -> labelrange : {cllist, str_of('$1'), [('$1')]}.
cllist -> labelrange t_comma cllist : {cllist, str_of('$1'),[('$1')] ++ value_of('$3')}.

% LabelRange = label [".." label].
labelrange -> label : {labelrange, str_of('$1'), {'$1'}}.
labelrange -> label t_ddot label: {labelrange, str_of('$1'), {'$1', '$3'}}.

% label = integer | string | qualident.
label -> integer : {label, str_of('$1'), '$1'}.
label -> string : {label, str_of('$1'), '$1'}.
label -> qualident : {label, str_of('$1'), '$1'}.

% TODO
% WhileStatement = WHILE expression DO StatementSequence {ELSIF expression DO StatementSequence} END.

% TODO
% RepeatStatement = REPEAT StatementSequence UNTIL expression.

% TODO
% ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression] DO StatementSequence END.



Erlang code.

list_tail({_, List}) -> List.

str_of(Token) ->
    element(2, Token).

value_of(Token) ->
    element(3, Token).