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
number mul_operator import add_operator
qualident basetype label relation label_range identdef
case_label_list case_label_list_rep ident_list_rep ident_list
import_list_rep import_list formal_type formal_type_rep
fpsection fpsection_ type struc_type pointer_type
const_expression expression length variable_declaration
field_list const_declaration type_declaration
assignment procedure_type struct_type array_type record_type
ntcase field_list_sequence fpsection_ident_rep
statement simple_expression for_statement base_type
procedure_heading statement_sequence procedure_declaration
formal_parameters factor actual_parameters declaration_sequence
selector designator exp_list procedure_call procedure_body
if_statement case_statement while_statement repeat_statement
element set term simple_expression
module root_def
.

Terminals
integer_dec integer_hex letter digit real string
t_mul t_divide t_div t_mod t_and t_assign
t_plus t_minus t_or t_dot t_equ t_sharp t_less t_lesseq t_more t_moreeq t_in t_is
t_ddot t_comma t_import implist t_semicolon t_array t_of t_colon t_var
t_for t_to t_by t_do t_end t_record t_rpar t_lpar t_procedure t_module 
t_tilda t_nil t_true t_false t_begin t_lbrack t_rbrack t_arrow t_pointer
t_repeat t_until ident t_zalupa
.

Rootsymbol root_def .


% module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .
module -> t_module ident t_semicolon import_list declaration_sequence t_begin statement_sequence t_end ident t_dot : 
 {module, str_of('$1'), {'$2', '$4', '$5', '$7', '$9'}}.
module -> t_module ident t_semicolon declaration_sequence t_begin statement_sequence t_end ident t_dot : 
 {module, str_of('$1'), {'$2',  nil, '$4', '$6', '$8'}}.
module -> t_module ident t_semicolon declaration_sequence t_end ident t_dot : 
 {module, str_of('$1'), {'$2',  nil, '$4',  nil, '$6'}}.

% без DeclarationSequence который может быть пустым вообще
module -> t_module ident t_semicolon import_list t_begin statement_sequence t_end ident t_dot : 
 {module, str_of('$1'), {'$2', '$4',  nil, '$6', '$8'}}.
module -> t_module ident t_semicolon t_begin statement_sequence t_end ident t_dot : 
 {module, str_of('$1'), {'$2',  nil,  nil, '$5', '$7'}}.
module -> t_module ident t_semicolon t_end ident t_dot : 
 {module, str_of('$1'), {'$2',  nil,  nil,  nil, '$5'}}.



% ident -> letter : {ident, str_of('$1'), value_of('$1')}.
% ident -> letter ident_rep : {ident, str_of('$1'), value_of('$1')++value_of('$2')}.
% ident_rep -> letter : '$1'.
% ident_rep -> integer_dec : '$1'.
% ident_rep -> integer_hex : '$1'.
% ident_rep -> ident_rep letter : {ident, str_of('$1'), value_of('$1')++value_of('$2')}.
% ident_rep -> ident_rep integer_dec : {ident, str_of('$1'), value_of('$1')++value_of('$2')}.
% ident_rep -> ident_rep integer_hex : {ident, str_of('$1'), value_of('$1')++value_of('$2')}.

number -> integer_dec : {number, str_of('$1'), '$1'}.
number -> integer_hex : {number, str_of('$1'), '$1'}.
number -> real : {number, str_of('$1'), '$1'}.

% +MulOperator = "*" | "/" | DIV | MOD | "&".
mul_operator -> t_mul : {mul_operator, str_of('$1'), '$1'}.
mul_operator -> t_divide : {mul_operator, str_of('$1'), '$1'}.
mul_operator -> t_div : {mul_operator, str_of('$1'), '$1'}.
mul_operator -> t_mod : {mul_operator, str_of('$1'), '$1'}.
mul_operator -> t_and : {mul_operator, str_of('$1'), '$1'}.

% +AddOperator = "+" | "-" | OR.
add_operator -> t_plus : {add_operator, str_of('$1'), '$1'}.
add_operator -> t_minus : {add_operator, str_of('$1'), '$1'}.
add_operator -> t_or : {add_operator, str_of('$1'), '$1'}.

% +import = ident [":=" ident].
import -> ident t_assign ident: {import, str_of('$1'), {'$1', '$3'}}.
import -> ident : {import, str_of('$1'), '$1'}.

% qualident = [ident "."] ident.
qualident -> ident t_dot ident : {qualident, str_of('$1'), {'$1', '$3'}}.
qualident -> ident : {qualident, str_of('$1'), '$1'}.

% BaseType = qualident.
basetype -> qualident : {basetype, str_of('$1'), '$1'}.

% label = integer | string | qualident.
label -> qualident : {label, str_of('$1'), '$1'}.
label -> string : {label, str_of('$1'), '$1'}.
label -> integer_dec : {label, str_of('$1'), '$1'}.
label -> integer_hex : {label, str_of('$1'), '$1'}.

% +relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
relation -> t_equ : {relation, str_of('$1'), '$1'}.
relation -> t_sharp : {relation, str_of('$1'), '$1'}.
relation -> t_less : {relation, str_of('$1'), '$1'}.
relation -> t_lesseq : {relation, str_of('$1'), '$1'}.
relation -> t_more : {relation, str_of('$1'), '$1'}.
relation -> t_moreeq : {relation, str_of('$1'), '$1'}.
relation -> t_in : {relation, str_of('$1'), '$1'}.
relation -> t_is : {relation, str_of('$1'), '$1'}.

% LabelRange = label [".." label].
label_range -> label t_ddot label: {label_range, str_of('$1'), {'$1', '$3'}}.
label_range -> label : {label_range, str_of('$1'), '$1'}.

% +identdef = ident ["*"].
identdef -> ident t_mul : {identdef, str_of('$1'), value_of('$1')++"*"}.
identdef -> ident       : {identdef, str_of('$1'), value_of('$1')}.

% +CaseLabelList = LabelRange {"," LabelRange}.
case_label_list -> case_label_list_rep : '$1'.
case_label_list_rep -> label_range : {case_label_list, str_of('$1'), [('$1')]}.
case_label_list_rep -> label_range t_comma case_label_list_rep : {case_label_list, str_of('$1'),[('$1')] ++ value_of('$3')}.

% +IdentList = identdef {"," identdef}.
ident_list -> ident_list_rep : '$1'.
ident_list_rep -> identdef : {idlist, str_of('$1'), ['$1']}.
ident_list_rep -> identdef t_comma ident_list_rep: {idlist, str_of('$1'), ['$1'] ++ value_of('$3')}.

% +ImportList = IMPORT import {"," import} ";".
import_list -> t_import import import_list_rep t_semicolon : {import_list, str_of('$1'), ['$2'] ++ value_of('$3')}.
import_list -> t_import import t_semicolon : {import_list, str_of('$1'), ['$2']}.
import_list_rep -> t_comma import : {idlist, str_of('$1'), ['$2']}.
import_list_rep -> import_list_rep t_comma import  : {idlist, str_of('$1'), value_of('$1') ++ ['$3']}.

% +FormalType = {ARRAY OF} qualident.
formal_type -> formal_type_rep : {formal_type, str_of('$1'), '$1'}.
formal_type_rep -> qualident : '$1'.
formal_type_rep -> t_array t_of formal_type_rep : {array_of, str_of('$3'), '$3'}.

% +FPSection = [VAR] ident {"," ident} ":" FormalType.
% +FPSection = [VAR] fpsection_ident_rep ":" FormalType.
fpsection -> t_var fpsection_ident_rep t_colon formal_type : {fpsection, str_of('$2'), {var, '$2','$4'}}.
fpsection -> fpsection_ident_rep t_colon formal_type : {fpsection, str_of('$1'), {not_var, '$1','$3'}}.
fpsection_ident_rep -> ident : {fpsection_ident_rep, str_of('$1'), ['$1']}.
fpsection_ident_rep -> fpsection_ident_rep t_comma ident : {fpsection_ident_rep, str_of('$1'), value_of('$1') ++ ['$3']}.

% % +type = qualident | StrucType.
type -> qualident : {type, str_of('$1'), '$1'}.
type -> struc_type : {type, str_of('$1'), '$1'}.

% PointerType = POINTER TO type.
pointer_type -> t_pointer t_to type : {pointer_type, str_of('$1'), '$3'}.

% ConstExpression = expression.
const_expression -> expression : {pointer_type, str_of('$1'), '$1'}.

% length = ConstExpression.
length -> const_expression : {length, str_of('$1'), '$1'}.

% VariableDeclaration = IdentList ":" type.
variable_declaration -> ident_list t_colon type : {variable_declaration, str_of('$1'), {'$1', '$3'}}.

% FieldList = IdentList ":" type.
field_list -> ident_list t_colon type : {field_list, str_of('$1'), {'$1', '$3'}}.

% ConstDeclaration = identdef "=" ConstExpression.
const_declaration -> identdef t_equ const_expression : {fielconst_declarationd_list, str_of('$1'), {'$1', '$3'}}.

% TypeDeclaration = identdef "=" StrucType.
type_declaration -> identdef t_equ struc_type : {type_declaration, str_of('$1'), {'$1', '$3'}}.

% assignment = designator ":=" expression.
assignment -> designator t_assign expression : {assignment, str_of('$1'), {'$1', '$3'}}.

% ProcedureType = PROCEDURE [FormalParameters].
procedure_type -> t_procedure : {procedure_type, str_of('$1'), nil}.
procedure_type -> t_procedure formal_parameters: {procedure_type, str_of('$1'), '$2'}

% StrucType = ArrayType | RecordType | PointerType | ProcedureType.
struct_type -> array_type : {struct_type, str_of('$1'), '$1'}.
struct_type -> record_type : {struct_type, str_of('$1'), '$1'}.
struct_type -> pointer_type : {struct_type, str_of('$1'), '$1'}.
struct_type -> procedure_type : {struct_type, str_of('$1'), '$1'}.

% element = expression [".." expression].
element -> expression : {element, str_of('$1'), '$1'}.
element -> expression t_ddot expression: {element, str_of('$1'), {'$1', '$3'}}.

% ProcedureCall = designator [ActualParameters].
procedure_call -> designator actual_parameters : {procedure_call, str_of('$1'), '$2'}.
procedure_call -> designator : {procedure_call, str_of('$1'), nil}.

% TODO! case = [CaseLabelList ":" StatementSequence].
% ntcase -> '$empty' : 'Elixir.T':new({ntcase, nil, nil}).
ntcase -> case_label_list t_colon statement_sequence : {procedure_call, str_of('$1'), {'$1', '$3'}}.

% selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
selector -> t_dot ident : {procedure_call, str_of('$1'), {'$1', '$2'}}.
selector -> t_lbrack exp_list t_rbrack : {procedure_call, str_of('$1'), {'$1', '$2', '$3'}}.
selector -> t_arrow : {procedure_call, str_of('$1'), '$1'}.
selector -> t_lpar exp_list t_rpar : {procedure_call, str_of('$1'), {'$1', '$2', '$3'}}.

% factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
factor -> number : {factor, str_of('$1'), '$1'}.
factor -> string : {factor, str_of('$1'), '$1'}.
factor -> t_nil : {factor, str_of('$1'), '$1'}.
factor -> t_true : {factor, str_of('$1'), '$1'}.
factor -> t_false : {factor, str_of('$1'), '$1'}.
factor -> set : {factor, str_of('$1'), '$1'}.
factor -> designator : {factor, str_of('$1'), '$1'}.
factor -> designator actual_parameters: {factor, str_of('$1'), {'$1', '$2'}}.
factor -> t_lpar expression t_rpar : {factor, str_of('$1'), {'$1', '$2', '$3'}}.
factor -> t_tilda factor : {factor, str_of('$1'), {'$1', '$2'}}.

% ActualParameters = "(" [ExpList] ")" .
actual_parameters -> t_lpar t_rpar : {actual_parameters, str_of('$1'), nil}.
actual_parameters -> t_lpar exp_list t_rpar : {actual_parameters, str_of('$1'), '$2'}.

% RepeatStatement = REPEAT StatementSequence UNTIL expression.
repeat_statement -> t_repeat statement_sequence t_until expression : 
 {repeat_statement, str_of('$1'), {'$2', '$4'}}.

% expression = SimpleExpression [relation SimpleExpression].
expression -> simple_expression : {expression, str_of('$1'), '$1'}.
expression -> simple_expression relation simple_expression: {expression, str_of('$1'), {'$1', '$3'}}.

% TODO! statement = [assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement].
statement -> assignment       : {statement, str_of('$1'), '$1'}.
statement -> procedure_call   : {statement, str_of('$1'), '$1'}.
statement -> if_statement     : {statement, str_of('$1'), '$1'}.
statement -> case_statement   : {statement, str_of('$1'), '$1'}.
statement -> while_statement  : {statement, str_of('$1'), '$1'}.
statement -> repeat_statement : {statement, str_of('$1'), '$1'}.
statement -> for_statement    : {statement, str_of('$1'), '$1'}.

% ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression] DO StatementSequence END.
for_statement -> t_for ident t_assign expression t_to expression t_by const_expression t_do statement_sequence t_end : 
  {for_statement, str_of('$1'), {'$2', '$4', '$6', '$8', '$10'}}.
for_statement -> t_for ident t_assign expression t_to expression t_do statement_sequence t_end : 
  {for_statement, str_of('$1'), {'$2', '$4', '$6',  nil, '$8'}}.

% ProcedureHeading = PROCEDURE identdef [FormalParameters].
procedure_heading -> t_procedure identdef : {procedure_heading, str_of('$1'), {'$2', nil}}.
procedure_heading -> t_procedure identdef formal_parameters : {procedure_heading, str_of('$1'), {'$2', '$3'}}.

% ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
procedure_declaration -> procedure_heading t_semicolon procedure_body ident : 
  {procedure_declaration, str_of('$1'), {'$1', '$3', '$4'}}.

% RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
record_type -> t_record t_lpar base_type t_rpar field_list_sequence t_end : 
  {procedure_declaration, str_of('$1'), {'$3', '$5'}}.
record_type -> t_record t_lpar base_type t_rpar t_end : 
  {procedure_declaration, str_of('$1'), {'$3', nil}}.
record_type -> t_record field_list_sequence t_end : 
  {procedure_declaration, str_of('$1'), {nil, '$2'}}.
record_type -> t_record t_end : 
  {procedure_declaration, str_of('$1'), {nil, nil}}.

% SimpleExpression = ["+" | "-"] term {AddOperator term}.
simple_expression -> term : {simple_expression, str_of('$1'), {nil, '$1', nil, nil}}.
simple_expression -> t_plus term : {simple_expression, str_of('$1'), {t_plus, '$1', nil, nil}}.
simple_expression -> t_minus term : {simple_expression, str_of('$1'), {t_minus, '$1', nil, nil}}.
% simple_expression -> term addoperator simpleexpression : {simpleexpression_list, str_of('$2'), ['$1'] ++ value_of('$3') ++ ['$2']}.
% simple_expression -> t_plus term addoperator simpleexpression : {simpleexpression_list, str_of('$3'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_plus, str_of('$1'), value_of('$1')})] ++ value_of('$4') ++ ['$3']}.
% simple_expression -> t_minus term addoperator simpleexpression : {simpleexpression_list, str_of('$3'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_minus, str_of('$1'), value_of('$1')})] ++ value_of('$4') ++ ['$3']}.

% term = factor {MulOperator factor}.
term -> factor : {term, str_of('$1'), '$1'}.

% root_def -> term : '$1'.
% root_def -> simple_expression : '$1'.
root_def -> module : '$1'.

declaration_sequence -> t_zalupa : nil.
field_list_sequence -> t_zalupa : nil.
formal_parameters -> t_zalupa : nil.
set -> t_zalupa : nil.
array_type -> t_zalupa : nil.
procedure_body -> t_zalupa : nil.
designator -> t_zalupa : nil.
exp_list -> t_zalupa : nil.
statement_sequence -> t_zalupa : nil.
if_statement -> t_zalupa : nil.
case_statement -> t_zalupa : nil.
while_statement -> t_zalupa : nil.

% % +ProcedureCall = designator [ActualParameters].
% procedurecall -> designator actualparameters : 'Elixir.T':new({procedurecall, str_of('$1'), {'$1', value_of('$1') }}).
% procedurecall -> designator : 'Elixir.T':new({procedurecall, str_of('$1'), {'$1', nil }}).

% % +designator = qualident {selector}.
% designator -> qualident selector: {deslist, str_of('$1'), ['Elixir.T':new({ident, str_of('$1'), value_of('$1')})]++ '$2'}.
% designator -> qualident : 'Elixir.T':new({designator, str_of('$1'), value_of('$1')}).

% % +number = integer | real.
% number -> integer : 'Elixir.T':new('$1').
% number -> real : 'Elixir.T':new('$1').
% number -> character : 'Elixir.T':new('$1').

% module -> selectorarr : '$1'.
% % +module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .
% % module -> t_module ident t_semicolon module_importlist declarationsequence module_begin t_end ident t_dot : 'Elixir.T':new({module, nil, {'$2', '$4', '$5', '$6', '$8'}}).
% module_importlist -> '$empty' : nil.
% module_importlist -> importlist : '$1'.

% module_begin -> '$empty' : nil.
% module_begin -> t_begin statementsequence : '$2'.

% % +ImportList = IMPORT import {"," import} ";".
% importlist -> t_import implist t_semicolon : 'Elixir.T':new({importlist, str_of('$1'), '$2'}).
% implist -> import : ['$1'].
% implist -> import t_comma implist : ['$1'] ++ '$3'.
 
% % +import = ident [":=" ident].
% import -> ident : 'Elixir.T':new({import, str_of('$1'), {value_of('$1'), value_of('$1')}}).
% import -> ident t_assign ident: 'Elixir.T':new({import, str_of('$1'), {value_of('$1'), value_of('$3')}}).

% % +DeclarationSequence = [CONST {ConstDeclaration ";"}] [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}] {ProcedureDeclaration ";"}.
% declarationsequence -> declarationsequence_const declarationsequence_type declarationsequence_var proc_decl_li : 'Elixir.T':new({declarationsequence, nil, {'$1', '$2', '$3', '$4'}}).
% % declarationsequence -> declarationsequence_const declarationsequence_type declarationsequence_var proc_decl_li : #{?MType => 'Elixir.DECLARATION_SEQUENCE', constant_declaration => '$1', type_declaration => '$2', variable_declaration => '$3', procedure_declaration => '$4'}.

% %   [CONST {ConstDeclaration ";"}]
% declarationsequence_const -> '$empty' : nil.
% declarationsequence_const -> t_const const_decl_li : '$2'.
% const_decl_li -> '$empty' : [].
% const_decl_li -> constantdeclaration t_semicolon: ['$1'].
% const_decl_li -> const_decl_li constantdeclaration t_semicolon: '$1' ++ ['$2'].

% %   [TYPE {TypeDeclaration ";"}]
% declarationsequence_type -> '$empty' : nil.
% declarationsequence_type -> t_type type_decl_li : '$2'.
% type_decl_li -> '$empty' : [].
% type_decl_li -> typedeclaration t_semicolon: ['$1'].
% type_decl_li -> type_decl_li typedeclaration t_semicolon: '$1' ++ ['$2'].

% %   [VAR {VariableDeclaration ";"}]
% declarationsequence_var -> '$empty' : nil.
% declarationsequence_var -> t_var var_decl_li : 'Elixir.T':new({variabledeclaration, str_of('$1'), '$2'}).
% var_decl_li -> '$empty' : [].
% var_decl_li -> variabledeclaration t_semicolon: ['$1'].
% var_decl_li -> var_decl_li variabledeclaration t_semicolon: '$1' ++ ['$2'].

% %   {ProcedureDeclaration ";"}.
% proc_decl_li ->  '$empty' : [].
% proc_decl_li -> proceduredeclaration t_semicolon : ['$1'].
% proc_decl_li -> proc_decl_li proceduredeclaration t_semicolon : '$1' ++ ['$2'].

% % +ConstDeclaration = identdef "=" ConstExpression.
% constantdeclaration -> identdef t_equ constexpression : 'Elixir.T':new({constantdeclaration, str_of('$1'), {'$1', '$3'}}).

% % +ConstExpression = expression.
% constexpression -> expression : 'Elixir.T':new({constexpression, str_of('$1'), value_of('$1')}).

% % +TypeDeclaration = identdef "=" StrucType.
% typedeclaration -> identdef t_equ type : 'Elixir.T':new({typedeclaration, str_of('$1'), {'$1', '$3'}}).

% % +StrucType = ArrayType | RecordType | PointerType | ProcedureType.
% structype -> arraytype : 'Elixir.T':new({structype, str_of('$1'), '$1'}).
% structype -> recordtype : 'Elixir.T':new({structype, str_of('$1'), '$1'}).
% structype -> proceduretype : 'Elixir.T':new({structype, str_of('$1'), '$1'}).
% structype -> pointertype : 'Elixir.T':new({structype, str_of('$1'), '$1'}).

% % +ArrayType = ARRAY length {"," length} OF type.
% arraytype -> arraytypelist type : 'Elixir.T':new({arraytype, str_of('$1'), {'$1', '$2'}}).
% lenlist -> '$empty' : dynamic.
% lenlist -> length : ['$1'].
% lenlist -> length t_comma lenlist : ['$1'] ++ '$3'.

% arraytypelist -> t_array lenlist t_of arraytypelist : {arraytype, {'$4', ['$2']}}.
% arraytypelist -> t_array lenlist t_of : {arraytype, ['$2']}.

% % +length = ConstExpression.
% length -> constexpression : 'Elixir.T':new({length, str_of('$1'), '$1'}).

% % +RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
% recordtype -> t_record t_end : 'Elixir.T':new({recordtype, str_of('$1'), {nil, []}}).
% recordtype -> t_record fieldlistsequence t_end : 'Elixir.T':new({recordtype, str_of('$1'), {nil, '$2'}}).
% recordtype -> t_record t_lpar basetype t_rpar t_end : 'Elixir.T':new({recordtype, str_of('$1'), {value_of('$3'), []}}).
% recordtype -> t_record t_lpar basetype t_rpar fieldlistsequence t_end : 'Elixir.T':new({recordtype, str_of('$1'), {value_of('$3'), '$5'}}).

% % +BaseType = qualident.
% basetype -> qualident : {basetype, str_of('$1'), ('$1')}.

% % +FieldListSequence = FieldList {";" FieldList}. 
% fieldlistsequence -> fieldlistsequence_list : 'Elixir.T':new({fieldlistsequence, str_of('$1'), value_of('$1')}).

% fieldlistsequence_list -> fieldlist : {fieldlistsequence_list, str_of('$1'), ['$1']}.
% fieldlistsequence_list -> fieldlist t_semicolon : {fieldlistsequence_list, str_of('$1'), ['$1']}.
% fieldlistsequence_list -> fieldlist t_semicolon fieldlistsequence_list : {fieldlistsequence_list, str_of('$1'), ['$1'] ++ value_of('$3')}.
% % fieldlistsequence_list -> fieldlist t_semicolon fieldlistsequence_list t_semicolon : {fieldlistsequence_list, str_of('$1'), ['$1'] ++ value_of('$3')}.

% % +FieldList = IdentList ":" type.
% fieldlist -> identlist t_colon type : 'Elixir.T':new({fieldlist, str_of('$1'), {value_of('$1'), value_of('$3')}}).

% % +IdentList = identdef {"," identdef}.
% identlist -> idlist : 'Elixir.T':new({identlist, str_of('$1'), value_of('$1')}).
% idlist -> identdef : {idlist, str_of('$1'), ['$1']}.
% idlist -> identdef t_comma idlist: {idlist, str_of('$1'), ['$1'] ++ value_of('$3')}.

% % +PointerType = POINTER TO type.
% pointertype -> t_pointer t_to type : 'Elixir.T':new({pointertype, str_of('$1'), '$3'}).

% % +ProcedureType = PROCEDURE [FormalParameters].
% proceduretype -> t_procedure : 'Elixir.T':new({proceduretype, str_of('$1'), []}).
% proceduretype -> t_procedure formalparameters: 'Elixir.T':new({proceduretype, str_of('$1'), '$2'}).

% % +FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
% formalparameters -> t_lpar fpseclist t_rpar t_colon proctype : 'Elixir.T':new({formalparameters, str_of('$1'), {'$2', '$5'}}).
% formalparameters -> t_lpar fpseclist t_rpar : 'Elixir.T':new({formalparameters, str_of('$1'), {'$2', []}}).
% formalparameters -> t_lpar t_rpar t_colon proctype : 'Elixir.T':new({formalparameters, str_of('$1'), {'$2', '$4'}}).

% proctype -> qualident : '$1'.
% proctype -> simpletype : '$1'.

% fpseclist -> fpsection : ['$1'].
% fpseclist -> fpsection t_semicolon fpseclist : ['$1'] ++ '$3'.

% % +FPSection = [VAR] ident {"," ident} ":" FormalType.
% fpsection -> idlist2 t_colon formaltype : 'Elixir.T':new({fpsection, str_of('$1'), {not_var, value_of('$1'), '$3'}}).
% fpsection -> t_var idlist2 t_colon formaltype : 'Elixir.T':new({fpsection, str_of('$1'), {var, value_of('$2'), '$4'}}).

% idlist2 -> ident : {idlist2, str_of('$1'), ['Elixir.T':new('$1')]}.
% idlist2 -> ident t_comma idlist2 : {idlist2, str_of('$1'), ['Elixir.T':new('$1')] ++ value_of('$3')}.

% % +FormalType = {ARRAY OF} qualident.
% formaltype -> qualident : 'Elixir.T':new({formaltype, str_of('$1'), {'$1', 0}}).
% %formaltype -> t_array t_of qualident : 'Elixir.T':new({formaltype, str_of('$1'), {'$2', value_of('$1')}}).
% formaltype -> simpletype : 'Elixir.T':new({formaltype, str_of('$1'), {'$1', 0}}).
% %formaltype -> t_array t_of simpletype : 'Elixir.T':new({formaltype, str_of('$1'), {'$2', value_of('$1')}}).
% formaltype -> arraytype : '$1'.

% t_array_list -> t_array t_of : {t_array_list, str_of('$1'), 1}.
% t_array_list -> t_array_list t_array t_of : {t_array_list, str_of('$1'), value_of('$1') + 1}.

% % +qualident = [ident "."] ident.
% qualident -> ident t_dot ident: 'Elixir.T':new({qualident, str_of('$1'), value_of('$1') ++ "." ++ value_of('$3')}).
% qualident -> ident : 'Elixir.T':new({qualident, str_of('$1'), value_of('$1')}).

% % +identdef = ident ["*"].
% identdef -> ident       : 'Elixir.T':new({identdef, str_of('$1'), value_of('$1')}).
% identdef -> ident t_mul : 'Elixir.T':new({identdef, str_of('$1'), value_of('$1') ++ "*"}).

% % +VariableDeclaration = IdentList ":" type. 
% variabledeclaration -> identlist t_colon type : 'Elixir.T':new({variabledeclaration, str_of('$1'), {'$1', '$3'}}).


% % +type = qualident | StrucType.
% type -> qualident : 'Elixir.T':new({type, str_of('$1'), '$1'}).
% type -> structype : 'Elixir.T':new({type, str_of('$1'), '$1'}).
% type -> simpletype : '$1'.

% % simpletype
% simpletype -> t_integer : '$1'.
% simpletype -> t_boolean : '$1'.
% simpletype -> t_byte : '$1'.
% simpletype -> t_char : '$1'.
% simpletype -> t_real : '$1'.

% % assertdeclaration
% assertdeclaration -> t_assert t_lpar expression t_rpar : '$3'. 

% % +ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
% proceduredeclaration -> procedureheading t_semicolon procedurebody ident : 'Elixir.T':new({proceduredeclaration, str_of('$1'), {'$1', '$3', 'Elixir.T':new('$4')}}).

% % +ProcedureHeading = PROCEDURE identdef [FormalParameters].
% procedureheading -> t_procedure identdef : 'Elixir.T':new({procedureheading, str_of('$1'), {'$2', []}}).
% procedureheading -> t_procedure identdef formalparameters : 'Elixir.T':new({procedureheading, str_of('$1'), {'$2', '$3'}}).

% % +ProcedureBody = DeclarationSequence [BEGIN StatementSequence] [RETURN expression] END.
% procedurebody -> declarationsequence procedurebody_stat_seq procedurebody_ret_exp t_end : 'Elixir.T':new({procedurebody, str_of('$1'), {'$1', '$2', '$3'}}).
% procedurebody_stat_seq -> '$empty' : nil.
% procedurebody_stat_seq -> t_begin statementsequence : '$2'.
% procedurebody_ret_exp  -> '$empty' : nil.
% procedurebody_ret_exp  -> t_return expression : '$2'.

% % +expression = SimpleExpression [relation SimpleExpression].
% expression -> simpleexpression : 'Elixir.T':new({expression, str_of('$1'), [('$1')]}).
% expression -> simpleexpression relation simpleexpression: 'Elixir.T':new({expression, str_of('$1'), ['$1', '$3', '$2']}).

% % +relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
% relation -> t_equ : 'Elixir.T':new('$1').
% relation -> t_sharp : 'Elixir.T':new('$1').
% relation -> t_less : 'Elixir.T':new('$1').
% relation -> t_lesseq : 'Elixir.T':new('$1').
% relation -> t_more : 'Elixir.T':new('$1').
% relation -> t_moreeq : 'Elixir.T':new('$1').
% relation -> t_in : 'Elixir.T':new('$1').
% relation -> t_is : 'Elixir.T':new('$1').

% % +SimpleExpression = ["+" | "-"] term {AddOperator term}.
% simpleexpression -> term : {simpleexpression_list, str_of('$1'), ['$1']}.
% simpleexpression -> t_plus term : {simpleexpression_list, str_of('$2'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_plus, str_of('$1'), value_of('$1')})]}.
% simpleexpression -> t_minus term : {simpleexpression_list, str_of('$2'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_minus, str_of('$1'), value_of('$1')})]}.
% simpleexpression -> term addoperator simpleexpression : {simpleexpression_list, str_of('$2'), ['$1'] ++ value_of('$3') ++ ['$2']}.
% simpleexpression -> t_plus term addoperator simpleexpression : {simpleexpression_list, str_of('$3'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_plus, str_of('$1'), value_of('$1')})] ++ value_of('$4') ++ ['$3']}.
% simpleexpression -> t_minus term addoperator simpleexpression : {simpleexpression_list, str_of('$3'), ['$2', 'Elixir.UNARY_OPERATOR':new({t_minus, str_of('$1'), value_of('$1')})] ++ value_of('$4') ++ ['$3']}.

% % +AddOperator = "+" | "-" | OR.
% addoperator -> t_plus : 'Elixir.T':new('$1').
% addoperator -> t_minus : 'Elixir.T':new('$1').
% addoperator -> t_or : 'Elixir.T':new('$1').

% % +term = factor {MulOperator factor}.
% % term -> factor : {term, str_of('$1'), value_of('$1')}.
% % term -> term muloperator term: {term, str_of('$1'), {'$2', value_of('$1'), value_of('$3')}}.
% % term -> termlist : 'Elixir.T':new({term, str_of('$1'), value_of('$1')}).
% term -> termlist : 'Elixir.T':new({term, str_of('$1'), value_of('$1')}).
% termlist -> factor : {termlist, str_of('$1'), ['$1']}.
% termlist -> termlist muloperator factor: {termlist, str_of('$1'), value_of('$1') ++['$3'] ++ ['$2']}.

% % +MulOperator = "*" | "/" | DIV | MOD | "&".
% muloperator -> t_mul : 'Elixir.T':new('$1').
% muloperator -> t_divide : 'Elixir.T':new('$1').
% muloperator -> t_div : 'Elixir.T':new('$1').
% muloperator -> t_mod : 'Elixir.T':new('$1').
% muloperator -> t_and : 'Elixir.T':new('$1').

% % +factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
% factor -> number : 'Elixir.T':new({factor, str_of('$1'), '$1'}).
% factor -> string : 'Elixir.T':new({factor, str_of('$1'), 'Elixir.T':new('$1')}).
% factor -> t_nil : 'Elixir.T':new({factor, str_of('$1'), 'Elixir.T':new('$1')}).
% factor -> t_true : 'Elixir.T':new({factor, str_of('$1'), 'Elixir.T':new('$1')}).
% factor -> t_false : 'Elixir.T':new({factor, str_of('$1'), 'Elixir.T':new('$1')}).
% factor -> set : 'Elixir.T':new({factor, str_of('$1'), '$1'}).
% factor -> designator : 'Elixir.T':new({factor, str_of('$1'), '$1'}).
% factor -> designator actualparameters: 'Elixir.T':new({factor, str_of('$1'), {'$1', '$2'}}).
% factor -> t_lpar expression t_rpar : 'Elixir.T':new({factor, str_of('$1'), '$2'}).
% factor -> t_tilda factor : 'Elixir.T':new({factor, str_of('$1'), {t_tilda, '$2'}}).

% selector -> t_dot ident : '$2'.
% selector -> selectorarr : '$1'.
% selector -> t_arrow : ['Elixir.UNARY_OPERATOR':new({t_arrow, str_of('$1'), value_of('$1')})].

% selectorarr -> t_lbrack explist t_rbrack selectorarr: {'$2', '$4'}.
% selectorarr -> t_lbrack explist t_rbrack : '$2'.
% % отключен, потому что ломает procedurecall actualparameters
% %selector -> t_lpar qualident t_rpar : {deselem, str_of('$1'), {pars_qualident, str_of('$1'), '$2'}}.

% %deslist2 -> '$empty' : [].
% %deslist2 -> deselem1 : '$1'.
% %deslist2 -> deselem2 : '$1'.
% %deslist2 -> deselem3 : '$1'.
% %deslist2 -> deselem4 : '$1'.
% %deslist2 -> deslist2 deselem1 : '$1' ++ '$2'.
% %deslist2 -> deslist2 deselem2 : '$1' ++ '$2'.
% %deslist2 -> deslist2 deselem3 : '$1' ++ '$2'.
% %deslist2 -> deslist2 deselem4 : '$1' ++ '$2'.

% %deselem1 -> t_dot ident : ['Elixir.UNARY_OPERATOR':new({t_arrow, str_of('$1'), value_of('$1')}), 'Elixir.T':new({ident, str_of('$1'), value_of('$2')})].
% %deselem2 -> t_lbrack explist t_rbrack : ['Elixir.T':new({explist, str_of('$1'), value_of('$2')})].
% %% deselem3 -> t_lpar qualident t_rpar : {deselem, str_of('$1'), {pars_qualident, str_of('$1'), '$2'}}.
% %% deselem3 -> t_lpar ident t_rpar : [{pars_qualident, str_of('$1'), '$2'}].
% %deselem3 -> actualparameters : ['$1'].
% %% тестовая хрень
% %% deselem3 -> t_lpar t_rpar : {deselem, str_of('$1'), {pars_qualident, str_of('$1'), 'nil'}}.
% %deselem4 -> t_arrow : ['Elixir.UNARY_OPERATOR':new({t_arrow, str_of('$1'), value_of('$1')})].

% % +set = "{" [element {"," element}] "}".
% set -> t_lbrace t_rbrace : 'Elixir.T':new({set, str_of('$1'), []}).
% set -> t_lbrace setlist t_rbrace : 'Elixir.T':new({set, str_of('$1'), '$2'}).
% setlist -> element : ['$1'].
% setlist -> element t_comma setlist : ['$1'] ++ '$3'.

% % +element = expression [".." expression].
% element -> expression : 'Elixir.T':new({element, str_of('$1'), ('$1')}).
% element -> expression t_ddot expression: 'Elixir.T':new({element, str_of('$1'), 'Elixir.ELEMENT_DIAPAZONE':new(('$1'), ('$3'))}).

% % +ExpList = expression {"," expression}.
% explist -> exlist : 'Elixir.T':new({explist, str_of('$1'), value_of('$1')}).
% exlist -> expression : {exlist, str_of('$1'), [('$1')]}.
% exlist -> expression t_comma exlist : {exlist, str_of('$1'),[('$1')] ++ value_of('$3')}.

% % +statement = [assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement].
% statement -> assignment      : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
% statement -> procedurecall   : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
% statement -> ifstatement     : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
% statement -> casestatement   : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
% statement -> whilestatement  : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
% statement -> repeatstatement : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
% statement -> forstatement    : 'Elixir.T':new({statement, str_of('$1'), '$1'}).
% statement -> assertdeclaration : 'Elixir.T':new({statement, str_of('$1'), '$1'}).

% % +assignment = designator ":=" expression.
% % assignment -> designator t_assign expression t_semicolon : 'Elixir.T':new({assignment, str_of('$1'), {'$1', '$3'}}).
% assignment -> designator t_assign expression : 'Elixir.T':new({assignment, str_of('$1'), {'$1', '$3'}}).


% % +StatementSequence = statement {";" statement}.
% statementsequence -> statement t_semicolon statementsequence : '$1'.
% statementsequence -> statement : {sslist, str_of('$1'), [value_of('$1')]}.
% statementsequence -> statement t_semicolon : {sslist, str_of('$1'), [value_of('$1')]}.


% % +IfStatement = IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence} [ELSE StatementSequence] END.
% ifstatement -> t_if expression t_then statementsequence t_end : '$1'.
% ifstatement -> t_if expression t_then statementsequence elsifseq t_end : '$1'.
% ifstatement -> t_if expression t_then statementsequence elsifseq ifelse t_end : '$1'.

% elsifseq -> t_elsif expression t_then statementsequence : '$1'.
% elsifseq -> t_elsif expression t_then statementsequence elsifseq : '$1'.

% ifelse -> t_else statementsequence : '$1'.

% %ifstatement -> t_if expression t_then statementsequence elsifsec ifelse t_end : 'Elixir.T':new({ifstatement, str_of('$1'), ['Elixir.T':new({va_t_if, '$2', '$4'})] ++ '$5' ++ '$6'}).
% %elsifsec -> '$empty' : [].
% %elsifsec -> t_elsif expression t_then statementsequence : ['Elixir.T':new({va_t_elsif, '$2', '$4'})].
% %elsifsec -> elsifsec t_elsif expression t_then statementsequence : '$1' ++ ['Elixir.T':new({va_t_elsif, '$3', '$5'})].
% %ifelse -> '$empty' : [].
% %ifelse -> t_else statementsequence : ['Elixir.T':new({va_t_else, '$2'})].

% % +CaseStatement = CASE expression OF case {"|" case} END.
% casestatement -> t_case expression t_of ntcaselist t_end : 'Elixir.T':new({casestatement, str_of('$1'), {'$2', value_of('$4')}}).
% ntcaselist -> ntcase : {ntcaselist, str_of('$1'), ['$1']}.
% ntcaselist -> ntcaselist t_vline ntcase : {ntcaselist, str_of('$1'), value_of('$1') ++ ['$3']}.

% % +case = [CaseLabelList ":" StatementSequence].
% ntcase -> '$empty' : 'Elixir.T':new({ntcase, nil, nil}).
% ntcase -> caselabellist t_colon statementsequence : 'Elixir.T':new({ntcase, str_of('$1'), {'$1', '$3'}}).

% % +CaseLabelList = LabelRange {"," LabelRange}.
% caselabellist -> cllist : 'Elixir.T':new({caselabellist, str_of('$1'), value_of('$1')}).
% cllist -> labelrange : {cllist, str_of('$1'), [('$1')]}.
% cllist -> labelrange t_comma cllist : {cllist, str_of('$1'),[('$1')] ++ value_of('$3')}.

% % +LabelRange = label [".." label].
% labelrange -> label : 'Elixir.T':new({labelrange, str_of('$1'), {'$1', '$1'}}).
% labelrange -> label t_ddot label: 'Elixir.T':new({labelrange, str_of('$1'), {'$1', '$3'}}).

% % +label = integer | string | qualident.
% label -> integer : 'Elixir.T':new({label, str_of('$1'), 'Elixir.T':new('$1')}).
% label -> string : 'Elixir.T':new({label, str_of('$1'), 'Elixir.T':new('$1')}).
% label -> qualident : 'Elixir.T':new({label, str_of('$1'), '$1'}).

% % +WhileStatement = WHILE expression DO StatementSequence {ELSIF expression DO StatementSequence} END.
% whilestatement -> t_while expression t_do statementsequence elsifdosec t_end: 'Elixir.T':new({whilestatement, str_of('$1'), ['Elixir.T':new({while_do, '$2', '$4'})] ++ '$5'}).
% elsifdosec -> '$empty' : [].
% elsifdosec -> t_elsif expression t_do statementsequence : ['Elixir.T':new({while_else_do, '$2', '$4'})].
% elsifdosec -> elsifdosec t_elsif expression t_do statementsequence : '$1' ++ ['Elixir.T':new({while_else_do, '$3', '$5'})].

% % +RepeatStatement = REPEAT StatementSequence UNTIL expression.
% repeatstatement -> t_repeat statementsequence t_until expression : 'Elixir.T':new({repeatstatement, str_of('$1'), {'$2', '$4'}}).

% % +ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression] DO StatementSequence END.
% forstatement -> t_for ident t_assign expression t_to expression forby t_do statementsequence t_end : 'Elixir.T':new({forstatement, str_of('$1'), {'$2', '$4', '$6', '$7', '$9'}}).
% forby -> '$empty' : nil.
% forby -> t_by constexpression : '$2'.

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

hex_or_letter({letter, Str, "A"}) -> {hex_digit, Str, "A"};
hex_or_letter({letter, Str, "B"}) -> {hex_digit, Str, "B"};
hex_or_letter({letter, Str, "C"}) -> {hex_digit, Str, "C"};
hex_or_letter({letter, Str, "D"}) -> {hex_digit, Str, "D"};
hex_or_letter({letter, Str, "E"}) -> {hex_digit, Str, "E"};
hex_or_letter({letter, Str, "F"}) -> {hex_digit, Str, "F"};
hex_or_letter(Other) -> Other.
