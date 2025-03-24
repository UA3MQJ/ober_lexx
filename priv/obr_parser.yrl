% http://oberon07.com/EBNF.txt
% https://models.molpit.org/oberon.html

% Для описания синтаксиса Оберона-2 используются Расширенные Бэкуса-Наура Формы (РБНФ). 
% Варианты разделяются знаком |. 
% Квадратные скобки [ и ] означают необязательность записанного внутри них выражения, 
% а фигурные скобки { и } означают его повторение (возможно 0 раз). 
% Нетерминальные символы начинаются с заглавной буквы (например, Оператор). 
% Терминальные символы или начинаются малой буквой (например, идент), 
% или записываются целиком заглавными буквами (например, BEGIN), или заключаются в кавычки (например, ":=").

Nonterminals 
module number muloperator addoperator relation import implist importlist identdef basetype
qualident qualident_one qualident_two
factor term simpleexpression simpleexpression_list expression constexpression constantdeclaration explist exlist
length element set setlist designator deselem1 deselem2 deselem3 deselem4 deslist deslist2 identlist idlist fieldlistsequence_list t_array_list
typedeclaration type simpletype structype arraytype lenlist fieldlist fieldlistsequence recordtype
pointertype variabledeclaration formaltype proceduretype fpsection idlist2 fpseclist formalparameters termlist
procedureheading label labelrange cllist caselabellist assignment procedurecall actualparameters statement statementsequence sslist
ntcase ntcaselist casestatement ifstatement elsifsec ifelse repeatstatement whilestatement elsifdosec
forstatement forby procedurebody declarationsequence procedurebody_stat_seq procedurebody_ret_exp proceduredeclaration
declarationsequence_const declarationsequence_type declarationsequence_var const_decl_li type_decl_li var_decl_li proc_decl_li
module_importlist module_begin
assertdeclaration
.

Terminals 
integer real character
ident t_import t_semicolon t_is t_in t_moreeq t_more t_lesseq t_less
t_sharp t_equ t_or t_minus t_plus t_and t_mod t_div t_divide t_mul t_assign t_comma t_dot
string t_nil t_true t_false t_tilda t_lpar t_rpar t_ddot t_lbrace t_rbrace t_arrow t_lbrack t_rbrack
t_array t_of t_end t_record t_colon t_pointer t_to t_var t_procedure t_vline t_case t_if t_then t_elsif t_else
t_repeat t_until t_while t_do t_for t_by t_begin t_return t_const t_type t_module
t_integer t_boolean t_byte t_char t_real
t_assert
.


% Nonassoc 950 const_decl_li.
% Nonassoc 940 type_decl_li.
% Nonassoc 930 var_decl_li.

% Nonassoc 850 declarationsequence_const.
% Nonassoc 840 declarationsequence_type.
% Nonassoc 830 declarationsequence_var.
% Nonassoc 820 proc_decl_li.

% Nonassoc 650 elsifsec.
% Nonassoc 640 ifelse.
% Nonassoc 630 elsifdosec.

% Nonassoc 570 t_dot.
% % Nonassoc 550 qualident.
% % % Nonassoc 560 ident.
% % % Nonassoc 550 qualident_two.
% % % Nonassoc 540 qualident_one.


% % % Nonassoc 300 deslist2.
% % % Left 550 deselem2.
% % % Left 540 formalparameters.

Rootsymbol module.

% module -> number : '$1'.
% module -> importlist : '$1'.
% module -> qualident : '$1'.
% module -> identdef : '$1'.
% module -> factor : '$1'.
% module -> set : '$1'.
% module -> term : '$1'.
% module -> statement : '$1'.
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
% module -> assignment : '$1'.
% module -> statementsequence : '$1'.
% module -> declarationsequence : '$1'.
% module -> procedurecall : '$1'.
% module -> statementsequence : '$1'.
% module -> ntcase : '$1'.
% module -> ntcaselist : '$1'.
% module -> casestatement : '$1'.
% module -> ifstatement : '$1'.
% module -> repeatstatement : '$1'.
% module -> whilestatement : '$1'.
% module -> forstatement : '$1'.
% module -> proceduredeclaration : '$1'.

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
number -> integer : 'Elixir.T':new('$1').
number -> real : 'Elixir.T':new('$1').
number -> character : 'Elixir.T':new('$1').

% +module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .
module -> t_module ident t_semicolon module_importlist declarationsequence module_begin t_end ident t_dot : 'Elixir.T':new({module, nil, {'$2', '$4', '$5', '$6', '$8'}}).
module_importlist -> '$empty' : nil.
module_importlist -> importlist : '$1'.

module_begin -> '$empty' : nil.
module_begin -> t_begin statementsequence : '$2'.

% +ImportList = IMPORT import {"," import} ";".
importlist -> t_import implist t_semicolon : 'Elixir.T':new({importlist, str_of('$1'), '$2'}).
implist -> import : ['$1'].
implist -> import t_comma implist : ['$1'] ++ '$3'.
 
% +import = ident [":=" ident].
import -> ident : 'Elixir.T':new({import, str_of('$1'), {value_of('$1'), value_of('$1')}}).
import -> ident t_assign ident: 'Elixir.T':new({import, str_of('$1'), {value_of('$1'), value_of('$3')}}).

% +DeclarationSequence = [CONST {ConstDeclaration ";"}] [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}] {ProcedureDeclaration ";"}.
declarationsequence -> declarationsequence_const declarationsequence_type declarationsequence_var proc_decl_li : 'Elixir.T':new({declarationsequence, nil, {'$1', '$2', '$3', '$4'}}).
% declarationsequence -> declarationsequence_const declarationsequence_type declarationsequence_var proc_decl_li : #{?MType => 'Elixir.DECLARATION_SEQUENCE', constant_declaration => '$1', type_declaration => '$2', variable_declaration => '$3', procedure_declaration => '$4'}.

%   [CONST {ConstDeclaration ";"}]
declarationsequence_const -> '$empty' : nil.
declarationsequence_const -> t_const const_decl_li : '$2'.
const_decl_li -> '$empty' : [].
const_decl_li -> constantdeclaration t_semicolon: ['$1'].
const_decl_li -> const_decl_li constantdeclaration t_semicolon: '$1' ++ ['$2'].

%   [TYPE {TypeDeclaration ";"}]
declarationsequence_type -> '$empty' : nil.
declarationsequence_type -> t_type type_decl_li : '$2'.
type_decl_li -> '$empty' : [].
type_decl_li -> typedeclaration t_semicolon: ['$1'].
type_decl_li -> type_decl_li typedeclaration t_semicolon: '$1' ++ ['$2'].

%   [VAR {VariableDeclaration ";"}]
declarationsequence_var -> '$empty' : nil.
declarationsequence_var -> t_var var_decl_li : 'Elixir.T':new({variabledeclaration, str_of('$1'), '$2'}).
var_decl_li -> '$empty' : [].
var_decl_li -> variabledeclaration t_semicolon: ['$1'].
var_decl_li -> var_decl_li variabledeclaration t_semicolon: '$1' ++ ['$2'].

%   {ProcedureDeclaration ";"}.
proc_decl_li ->  '$empty' : [].
proc_decl_li -> proceduredeclaration t_semicolon : ['$1'].
proc_decl_li -> proc_decl_li proceduredeclaration t_semicolon : '$1' ++ ['$2'].

% +ConstDeclaration = identdef "=" ConstExpression.
constantdeclaration -> identdef t_equ constexpression : 'Elixir.T':new({constantdeclaration, str_of('$1'), {'$1', '$3'}}).

% +ConstExpression = expression.
constexpression -> expression : 'Elixir.T':new({constexpression, str_of('$1'), value_of('$1')}).

% +TypeDeclaration = identdef "=" StrucType.
typedeclaration -> identdef t_equ type : 'Elixir.T':new({typedeclaration, str_of('$1'), {'$1', '$3'}}).

% +StrucType = ArrayType | RecordType | PointerType | ProcedureType.
structype -> arraytype : 'Elixir.T':new({structype, str_of('$1'), '$1'}).
structype -> recordtype : 'Elixir.T':new({structype, str_of('$1'), '$1'}).
structype -> proceduretype : 'Elixir.T':new({structype, str_of('$1'), '$1'}).
structype -> pointertype : 'Elixir.T':new({structype, str_of('$1'), '$1'}).

% +ArrayType = ARRAY length {"," length} OF type.
arraytype -> t_array lenlist t_of type : 'Elixir.T':new({arraytype, str_of('$1'), {'$2' ,'$4'}}).
lenlist -> length : ['$1'].
lenlist -> length t_comma lenlist : ['$1'] ++ '$3'.

% +length = ConstExpression.
length -> constexpression : 'Elixir.T':new({length, str_of('$1'), '$1'}).

% +RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
recordtype -> t_record t_end : 'Elixir.T':new({recordtype, str_of('$1'), {nil, []}}).
recordtype -> t_record fieldlistsequence t_end : 'Elixir.T':new({recordtype, str_of('$1'), {nil, '$2'}}).
recordtype -> t_record t_lpar basetype t_rpar t_end : 'Elixir.T':new({recordtype, str_of('$1'), {value_of('$3'), []}}).
recordtype -> t_record t_lpar basetype t_rpar fieldlistsequence t_end : 'Elixir.T':new({recordtype, str_of('$1'), {value_of('$3'), '$5'}}).

% +BaseType = qualident.
basetype -> qualident : {basetype, str_of('$1'), ('$1')}.

% +FieldListSequence = FieldList {";" FieldList}. 
fieldlistsequence -> fieldlistsequence_list : 'Elixir.T':new({fieldlistsequence, str_of('$1'), value_of('$1')}).

fieldlistsequence_list -> fieldlist : {fieldlistsequence_list, str_of('$1'), ['$1']}.
fieldlistsequence_list -> fieldlist t_semicolon : {fieldlistsequence_list, str_of('$1'), ['$1']}.
fieldlistsequence_list -> fieldlist t_semicolon fieldlistsequence_list : {fieldlistsequence_list, str_of('$1'), ['$1'] ++ value_of('$3')}.
% fieldlistsequence_list -> fieldlist t_semicolon fieldlistsequence_list t_semicolon : {fieldlistsequence_list, str_of('$1'), ['$1'] ++ value_of('$3')}.

% +FieldList = IdentList ":" type.
fieldlist -> identlist t_colon type : 'Elixir.T':new({fieldlist, str_of('$1'), {value_of('$1'), value_of('$3')}}).

% +IdentList = identdef {"," identdef}.
identlist -> idlist : 'Elixir.T':new({identlist, str_of('$1'), value_of('$1')}).
idlist -> identdef : {idlist, str_of('$1'), ['$1']}.
idlist -> identdef t_comma idlist: {idlist, str_of('$1'), ['$1'] ++ value_of('$3')}.

% +PointerType = POINTER TO type.
pointertype -> t_pointer t_to type : 'Elixir.T':new({pointertype, str_of('$1'), '$3'}).

% +ProcedureType = PROCEDURE [FormalParameters].
proceduretype -> t_procedure : 'Elixir.T':new({proceduretype, str_of('$1'), []}).
proceduretype -> t_procedure formalparameters: 'Elixir.T':new({proceduretype, str_of('$1'), '$2'}).

% +FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
formalparameters -> t_lpar t_rpar : 'Elixir.T':new({formalparameters, str_of('$1'), {[], nil}}).
formalparameters -> t_lpar t_rpar t_colon qualident : 'Elixir.T':new({formalparameters, str_of('$1'), {[], '$4'}}).
formalparameters -> t_lpar fpseclist t_rpar : 'Elixir.T':new({formalparameters, str_of('$1'), {'$2', nil}}).
formalparameters -> t_lpar fpseclist t_rpar t_colon ident : 'Elixir.T':new({formalparameters, str_of('$1'), {'$2', '$5'}}).

fpseclist -> fpsection : ['$1'].
fpseclist -> fpsection t_semicolon fpseclist : ['$1'] ++ '$3'.

% +FPSection = [VAR] ident {"," ident} ":" FormalType.
fpsection -> idlist2 t_colon formaltype : 'Elixir.T':new({fpsection, str_of('$1'), {not_var, value_of('$1'), '$3'}}).
fpsection -> t_var idlist2 t_colon formaltype : 'Elixir.T':new({fpsection, str_of('$1'), {var, value_of('$2'), '$4'}}).

idlist2 -> ident : {idlist2, str_of('$1'), ['Elixir.T':new('$1')]}.
idlist2 -> ident t_comma idlist2 : {idlist2, str_of('$1'), ['Elixir.T':new('$1')] ++ value_of('$3')}.

% +FormalType = {ARRAY OF} qualident.
formaltype -> qualident : 'Elixir.T':new({formaltype, str_of('$1'), {'$1', 0}}).
formaltype -> t_array t_of qualident : 'Elixir.T':new({formaltype, str_of('$1'), {'$2', value_of('$1')}}).
formaltype -> simpletype : 'Elixir.T':new({formaltype, str_of('$1'), {'$1', 0}}).
formaltype -> t_array t_of simpletype : 'Elixir.T':new({formaltype, str_of('$1'), {'$2', value_of('$1')}}).

t_array_list -> t_array t_of : {t_array_list, str_of('$1'), 1}.
t_array_list -> t_array_list t_array t_of : {t_array_list, str_of('$1'), value_of('$1') + 1}.

% +qualident = [ident "."] ident.
qualident -> qualident_two : '$1'.
qualident -> qualident_one : '$1'.
qualident_two -> ident t_dot ident: 'Elixir.T':new({qualident, str_of('$1'), value_of('$1') ++ "." ++ value_of('$3')}).
qualident_one -> ident : 'Elixir.T':new({qualident, str_of('$1'), value_of('$1')}).

% +identdef = ident ["*"].
identdef -> ident       : 'Elixir.T':new({identdef, str_of('$1'), value_of('$1')}).
identdef -> ident t_mul : 'Elixir.T':new({identdef, str_of('$1'), value_of('$1') ++ "*"}).

% +VariableDeclaration = IdentList ":" type. 
variabledeclaration -> identlist t_colon type : 'Elixir.T':new({variabledeclaration, str_of('$1'), {'$1', '$3'}}).


% +type = qualident | StrucType.
type -> qualident : 'Elixir.T':new({type, str_of('$1'), '$1'}).
type -> structype : 'Elixir.T':new({type, str_of('$1'), '$1'}).
type -> simpletype : '$1'.

% simpletype
simpletype -> t_integer : '$1'.
simpletype -> t_boolean : '$1'.
simpletype -> t_byte : '$1'.
simpletype -> t_char : '$1'.
simpletype -> t_real : '$1'.

% assertdeclaration
assertdeclaration -> t_assert t_lpar expression t_rpar : '$3'. 

% +ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
proceduredeclaration -> procedureheading t_semicolon procedurebody ident : 'Elixir.T':new({proceduredeclaration, str_of('$1'), {'$1', '$3', 'Elixir.T':new('$4')}}).

% +ProcedureHeading = PROCEDURE identdef [FormalParameters].
procedureheading -> t_procedure identdef : 'Elixir.T':new({procedureheading, str_of('$1'), {'$2', []}}).
procedureheading -> t_procedure identdef formalparameters : 'Elixir.T':new({procedureheading, str_of('$1'), {'$2', '$3'}}).

% +ProcedureBody = DeclarationSequence [BEGIN StatementSequence] [RETURN expression] END.
procedurebody -> declarationsequence procedurebody_stat_seq procedurebody_ret_exp t_end : 'Elixir.T':new({procedurebody, str_of('$1'), {'$1', '$2', '$3'}}).
procedurebody_stat_seq -> '$empty' : nil.
procedurebody_stat_seq -> t_begin statementsequence : '$2'.
procedurebody_ret_exp  -> '$empty' : nil.
procedurebody_ret_exp  -> t_return expression : '$2'.

% +expression = SimpleExpression [relation SimpleExpression].
expression -> simpleexpression : 'Elixir.T':new({expression, str_of('$1'), [('$1')]}).
expression -> simpleexpression relation simpleexpression: 'Elixir.T':new({expression, str_of('$1'), ['$1', '$3', '$2']}).

% +relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
relation -> t_equ : 'Elixir.T':new('$1').
relation -> t_sharp : 'Elixir.T':new('$1').
relation -> t_less : 'Elixir.T':new('$1').
relation -> t_lesseq : 'Elixir.T':new('$1').
relation -> t_more : 'Elixir.T':new('$1').
relation -> t_moreeq : 'Elixir.T':new('$1').
relation -> t_in : 'Elixir.T':new('$1').
relation -> t_is : 'Elixir.T':new('$1').

% +SimpleExpression = ["+" | "-"] term {AddOperator term}.
simpleexpression -> simpleexpression_list : 'Elixir.T':new({simpleexpression, str_of('$1'), value_of('$1')}).
simpleexpression_list -> term : {simpleexpression_list, str_of('$1'), ['$1']}.
simpleexpression_list -> t_plus term : {simpleexpression_list, str_of('$2'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_plus, str_of('$1'), value_of('$1')})]}.
simpleexpression_list -> t_minus term : {simpleexpression_list, str_of('$2'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_minus, str_of('$1'), value_of('$1')})]}.
simpleexpression_list -> term addoperator simpleexpression_list : {simpleexpression_list, str_of('$2'), ['$1'] ++ value_of('$3') ++ ['$2']}.
simpleexpression_list -> t_plus term addoperator simpleexpression_list : {simpleexpression_list, str_of('$3'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_plus, str_of('$1'), value_of('$1')})] ++ value_of('$4') ++ ['$3']}.
simpleexpression_list -> t_minus term addoperator simpleexpression_list : {simpleexpression_list, str_of('$3'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_minus, str_of('$1'), value_of('$1')})] ++ value_of('$4') ++ ['$3']}.

% +AddOperator = "+" | "-" | OR.
addoperator -> t_plus : 'Elixir.T':new('$1').
addoperator -> t_minus : 'Elixir.T':new('$1').
addoperator -> t_or : 'Elixir.T':new('$1').

% +term = factor {MulOperator factor}.
% term -> factor : {term, str_of('$1'), value_of('$1')}.
% term -> term muloperator term: {term, str_of('$1'), {'$2', value_of('$1'), value_of('$3')}}.
% term -> termlist : 'Elixir.T':new({term, str_of('$1'), value_of('$1')}).
term -> termlist : 'Elixir.T':new({term, str_of('$1'), value_of('$1')}).
termlist -> factor : {termlist, str_of('$1'), ['$1']}.
termlist -> termlist muloperator factor: {termlist, str_of('$1'), value_of('$1') ++['$3'] ++ ['$2']}.

% +MulOperator = "*" | "/" | DIV | MOD | "&".
muloperator -> t_mul : 'Elixir.T':new('$1').
muloperator -> t_divide : 'Elixir.T':new('$1').
muloperator -> t_div : 'Elixir.T':new('$1').
muloperator -> t_mod : 'Elixir.T':new('$1').
muloperator -> t_and : 'Elixir.T':new('$1').

% +factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
factor -> number : 'Elixir.T':new({factor, str_of('$1'), '$1'}).
factor -> string : 'Elixir.T':new({factor, str_of('$1'), 'Elixir.T':new('$1')}).
factor -> t_nil : 'Elixir.T':new({factor, str_of('$1'), 'Elixir.T':new('$1')}).
factor -> t_true : 'Elixir.T':new({factor, str_of('$1'), 'Elixir.T':new('$1')}).
factor -> t_false : 'Elixir.T':new({factor, str_of('$1'), 'Elixir.T':new('$1')}).
factor -> set : 'Elixir.T':new({factor, str_of('$1'), '$1'}).
factor -> designator : 'Elixir.T':new({factor, str_of('$1'), '$1'}).
factor -> designator actualparameters: 'Elixir.T':new({factor, str_of('$1'), {'$1', '$2'}}).
factor -> t_lpar expression t_rpar : 'Elixir.T':new({factor, str_of('$1'), '$2'}).
factor -> t_tilda factor : 'Elixir.T':new({factor, str_of('$1'), {t_tilda, '$2'}}).

% +designator = qualident {selector}.
% +selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
% designator  =  qualident {"." ident | "[" ExpList "]" | "(" qualident ")" | "^" }. 
% поправка
% designator = qualident {selector}.
% +selector = "." ident | "[" ExpList "]" | "^" | actualparameters.
designator -> deslist : 'Elixir.T':new({designator, str_of('$1'), value_of('$1')}).

% deslist -> qualident : {deslist, str_of('$1'), ['Elixir.T':new({qualident, str_of('$1'),value_of('$1')})]}.

deslist -> qualident : '$1'.

%deslist -> ident deslist2: {deslist, str_of('$1'), ['Elixir.T':new({ident, str_of('$1'), value_of('$1')})]++ '$2'}.
%
%deslist2 -> '$empty' : [].
%deslist2 -> deselem1 : '$1'.
%deslist2 -> deselem2 : '$1'.
%deslist2 -> deselem3 : '$1'.
%deslist2 -> deselem4 : '$1'.
%deslist2 -> deslist2 deselem1 : '$1' ++ '$2'.
%deslist2 -> deslist2 deselem2 : '$1' ++ '$2'.
%deslist2 -> deslist2 deselem3 : '$1' ++ '$2'.
%deslist2 -> deslist2 deselem4 : '$1' ++ '$2'.
%
%deselem1 -> t_dot ident : ['Elixir.UNARY_OPERATOR':new({t_arrow, str_of('$1'), value_of('$1')}), 'Elixir.T':new({ident, str_of('$1'), value_of('$2')})].
%deselem2 -> t_lbrack explist t_rbrack : ['Elixir.T':new({explist, str_of('$1'), value_of('$2')})].
%% deselem3 -> t_lpar qualident t_rpar : {deselem, str_of('$1'), {pars_qualident, str_of('$1'), '$2'}}.
%% deselem3 -> t_lpar ident t_rpar : [{pars_qualident, str_of('$1'), '$2'}].
%deselem3 -> actualparameters : ['$1'].
%% тестовая хрень
%% deselem3 -> t_lpar t_rpar : {deselem, str_of('$1'), {pars_qualident, str_of('$1'), 'nil'}}.
%deselem4 -> t_arrow : ['Elixir.UNARY_OPERATOR':new({t_arrow, str_of('$1'), value_of('$1')})].

% +set = "{" [element {"," element}] "}".
set -> t_lbrace t_rbrace : 'Elixir.T':new({set, str_of('$1'), []}).
set -> t_lbrace setlist t_rbrace : 'Elixir.T':new({set, str_of('$1'), '$2'}).
setlist -> element : ['$1'].
setlist -> element t_comma setlist : ['$1'] ++ '$3'.

% +element = expression [".." expression].
element -> expression : 'Elixir.T':new({element, str_of('$1'), ('$1')}).
element -> expression t_ddot expression: 'Elixir.T':new({element, str_of('$1'), 'Elixir.ELEMENT_DIAPAZONE':new(('$1'), ('$3'))}).

% +ExpList = expression {"," expression}.
explist -> exlist : 'Elixir.T':new({explist, str_of('$1'), value_of('$1')}).
exlist -> expression : {exlist, str_of('$1'), [('$1')]}.
exlist -> expression t_comma exlist : {exlist, str_of('$1'),[('$1')] ++ value_of('$3')}.

% +ActualParameters = "(" [ExpList] ")" .
actualparameters -> t_lpar t_rpar : 'Elixir.T':new({actualparameters, str_of('$1'), []}).
actualparameters -> t_lpar explist t_rpar : 'Elixir.T':new({actualparameters, str_of('$1'), value_of('$2')}).

% +statement = [assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement].
statement -> assignment      : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
statement -> procedurecall   : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
statement -> ifstatement     : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
statement -> casestatement   : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
statement -> whilestatement  : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
statement -> repeatstatement : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
statement -> forstatement    : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
statement -> assertdeclaration : 'Elixir.T':new({statement, str_of('$1'), '$1'}).

% +assignment = designator ":=" expression.
% assignment -> designator t_assign expression t_semicolon : 'Elixir.T':new({assignment, str_of('$1'), {'$1', '$3'}}).
assignment -> designator t_assign expression : 'Elixir.T':new({assignment, str_of('$1'), {'$1', '$3'}}).

% +ProcedureCall = designator [ActualParameters].
procedurecall -> designator : 'Elixir.T':new({procedurecall, str_of('$1'), {'$1', nil }}).
procedurecall -> designator actualparameters : 'Elixir.T':new({procedurecall, str_of('$1'), {'$1', '$2'}}).


% +StatementSequence = statement {";" statement}.
statementsequence -> sslist : 'Elixir.T':new({statementsequence, str_of('$1'), value_of('$1')}).

sslist -> statement : {sslist, str_of('$1'), [value_of('$1')]}.
sslist -> statement t_semicolon : {sslist, str_of('$1'), [value_of('$1')]}.

% может быть надо будет включить, если точка с запятой в конце - да. так и есть.
sslist -> statement t_semicolon sslist : {sslist, str_of('$1'), [value_of('$1')] ++ value_of('$3')}.

% +IfStatement = IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence} [ELSE StatementSequence] END.
ifstatement -> t_if expression t_then statementsequence elsifsec ifelse t_end : 'Elixir.T':new({ifstatement, str_of('$1'), ['Elixir.T':new({va_t_if, '$2', '$4'})] ++ '$5' ++ '$6'}).
elsifsec -> '$empty' : [].
elsifsec -> t_elsif expression t_then statementsequence : ['Elixir.T':new({va_t_elsif, '$2', '$4'})].
elsifsec -> elsifsec t_elsif expression t_then statementsequence : '$1' ++ ['Elixir.T':new({va_t_elsif, '$3', '$5'})].
ifelse -> '$empty' : [].
ifelse -> t_else statementsequence : ['Elixir.T':new({va_t_else, '$2'})].

% +CaseStatement = CASE expression OF case {"|" case} END.
casestatement -> t_case expression t_of ntcaselist t_end : 'Elixir.T':new({casestatement, str_of('$1'), {'$2', value_of('$4')}}).
ntcaselist -> ntcase : {ntcaselist, str_of('$1'), ['$1']}.
ntcaselist -> ntcaselist t_vline ntcase : {ntcaselist, str_of('$1'), value_of('$1') ++ ['$3']}.

% +case = [CaseLabelList ":" StatementSequence].
ntcase -> '$empty' : 'Elixir.T':new({ntcase, nil, nil}).
ntcase -> caselabellist t_colon statementsequence : 'Elixir.T':new({ntcase, str_of('$1'), {'$1', '$3'}}).

% +CaseLabelList = LabelRange {"," LabelRange}.
caselabellist -> cllist : 'Elixir.T':new({caselabellist, str_of('$1'), value_of('$1')}).
cllist -> labelrange : {cllist, str_of('$1'), [('$1')]}.
cllist -> labelrange t_comma cllist : {cllist, str_of('$1'),[('$1')] ++ value_of('$3')}.

% +LabelRange = label [".." label].
labelrange -> label : 'Elixir.T':new({labelrange, str_of('$1'), {'$1', '$1'}}).
labelrange -> label t_ddot label: 'Elixir.T':new({labelrange, str_of('$1'), {'$1', '$3'}}).

% +label = integer | string | qualident.
label -> integer : 'Elixir.T':new({label, str_of('$1'), 'Elixir.T':new('$1')}).
label -> string : 'Elixir.T':new({label, str_of('$1'), 'Elixir.T':new('$1')}).
label -> qualident : 'Elixir.T':new({label, str_of('$1'), '$1'}).

% +WhileStatement = WHILE expression DO StatementSequence {ELSIF expression DO StatementSequence} END.
whilestatement -> t_while expression t_do statementsequence elsifdosec t_end: 'Elixir.T':new({whilestatement, str_of('$1'), ['Elixir.T':new({while_do, '$2', '$4'})] ++ '$5'}).
elsifdosec -> '$empty' : [].
elsifdosec -> t_elsif expression t_do statementsequence : ['Elixir.T':new({while_else_do, '$2', '$4'})].
elsifdosec -> elsifdosec t_elsif expression t_do statementsequence : '$1' ++ ['Elixir.T':new({while_else_do, '$3', '$5'})].

% +RepeatStatement = REPEAT StatementSequence UNTIL expression.
repeatstatement -> t_repeat statementsequence t_until expression : 'Elixir.T':new({repeatstatement, str_of('$1'), {'$2', '$4'}}).

% +ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression] DO StatementSequence END.
forstatement -> t_for ident t_assign expression t_to expression forby t_do statementsequence t_end : 'Elixir.T':new({forstatement, str_of('$1'), {'$2', '$4', '$6', '$7', '$9'}}).
forby -> '$empty' : nil.
forby -> t_by constexpression : '$2'.

Erlang code.

-define(MType, '__struct__').

% list_tail({_, List}) -> List.
str_of(Obj) when is_tuple(Obj) -> tstr_of(Obj);
str_of(Obj) when is_map(Obj) -> mstr_of(Obj).

value_of(Obj) when is_tuple(Obj) -> tvalue_of(Obj);
value_of(Obj) when is_map(Obj) -> mvalue_of(Obj).

tstr_of(Token) ->
    % io:format('str_of ~w~n', [Token]),
    element(2, Token).

mstr_of(Map) ->
    % io:format('mstr_of ~w~n', [Map]),
    maps:get(str, Map, nil).

tvalue_of(Token) ->
    element(3, Token).

mvalue_of(Map) ->
    % io:format('value_of ~w~n', [Map]),
    maps:get(value, Map, nil).
