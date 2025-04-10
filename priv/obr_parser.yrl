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
root_def module 
statement_sequence factor statement_sequence_rep
assignment statement term term_rep expression
designator designator_rep relation
simple_expression simple_expression_pre simple_expression_rep
import_list import_list_rep import qualident
selector number 
t_begin_statement_sequence
selector_t_dot ident_t_dot
qualident_ident qualident_ident2 selector_ident add_operator mul_operator
exp_list exp_list_rep factor_expression
element set set_rep 
actual_parameters 
procedure_call procedure_call_parameters
selector_pars
if_statement if_statement_rep u_elsif t_else_statement_sequence
for_statement const_expression repeat_statement
while_statement while_statement_rep
case_statement_rep case_statement label label_range ntcase
case_label_list case_label_list_rep
declaration_sequence
ds_const_declaration ds_const_declaration_rep
ds_type_declaration ds_type_declaration_rep
ds_variable_declaration ds_variable_declaration_rep
const_declaration identdef
ds_procedure_declaration ds_procedure_declaration_rep
type_declaration variable_declaration
procedure_declaration type
formal_type formal_type_rep   
 ident_list_rep ident_list
fpsection fpsection_ident_rep
  pointer_type
  length 
field_list  field_list_sequence field_list_sequence_rep
procedure_type array_type 
 base_type
formal_parameters % formal_parameters_rep   
 array_type_rep 
%    
struct_type 
formal_parameters_qual_rep formal_parameters_fps_rep formal_parameters_fps_rep2
procedure_body_part1 procedure_body_part2
 procedure_body   
procedure_heading  
record_type record_type_part1 record_type_part2
% 
% 
.

Terminals
integer_dec integer_hex real string ident
t_module t_semicolon t_begin t_end t_dot 
t_equ t_assign t_nil t_true t_false t_comma
t_more t_moreeq t_import t_sharp t_less t_lesseq
t_in t_is t_plus t_minus t_or
t_mul t_divide t_div t_mod t_and 
t_arrow t_lpar t_rpar t_lbrack t_rbrack t_tilda
t_lbrace t_rbrace t_ddot
t_elseif t_elsif t_else t_then t_if
t_for t_to t_by t_do
t_repeat t_until t_while
t_case t_of t_colon t_vline t_const
t_type    t_array   t_var
 t_record   t_procedure  
     t_pointer
t_return   
%   
%  
.

Rootsymbol root_def .

Right    10 t_assign.

% Для qualident (неассоциативная точка)
% Nonassoc 20 ident_t_dot.

% Для selector (левоассоциативная точка)
% Left 30 selector_t_dot.

Unary 35 t_tilda.



% ProcedureCall = designator [ActualParameters].
%                     |              |
%                     |              +- ActualParameters = "(" [ExpList] ")"
% 				            |
% 					          +-- designator = qualident {selector}.
% 					                     |          |
% 										           |          +- selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
% 									          	 |
%                                +- qualident = [ident "."] ident.	
% поэтому вот такой приоритет. однако, не понятно
% как будет работать селектор со скобками, когда он
% действительно будет нужен

Nonassoc   80 import_list.
Nonassoc   90 import.
Nonassoc  180 statement_sequence.
Nonassoc  190 statement.
Nonassoc  200 assignment 
              procedure_call procedure_call_parameters
              if_statement if_statement_rep t_else_statement_sequence
              case_statement case_statement_rep
              while_statement while_statement_rep 
              repeat_statement for_statement.
Nonassoc  300 expression.
Nonassoc  305 relation.
Nonassoc  310 simple_expression.
Nonassoc  320 add_operator.
Nonassoc  330 term.
Nonassoc  340 mul_operator.
Nonassoc  350 factor.
Nonassoc  360 set set_rep 
              designator designator_rep.
Nonassoc  364 selector_pars.
Nonassoc  365 qualident.
Nonassoc  366 qualident_ident2.
Nonassoc  370 selector.
Nonassoc  370 selector_t_dot selector_ident.
Nonassoc  395 qualident_ident ident_t_dot.
Nonassoc  399 term_rep.
Nonassoc  400 actual_parameters.
Nonassoc  410 factor_expression.


Nonassoc  10000 t_begin  t_nil t_true t_false
  t_module t_semicolon t_end t_comma t_arrow. 

Nonassoc  10100 %ident 
  t_equ t_sharp 
  t_less t_lesseq t_more t_moreeq t_in t_is 
  t_lbrace t_rbrace t_ddot
  t_elseif t_elsif t_else t_then t_if
  t_for t_to t_by t_do
  t_repeat t_until t_while
  t_case t_of t_colon t_vline
  t_const t_type t_var
   t_array 
 t_record   t_procedure  
     t_pointer t_lpar t_rpar
t_return   t_lbrack t_rbrack
.

% Unary 500 'not'.
% Unary 400 '+'.
% Unary 400 '-'.
% Unary 300 '@'.


% root_def -> module : '$1'.
% root_def -> procedure_call_parameters : '$1'.
% root_def -> selector_pars : '$1'.
root_def -> statement : '$1'.


%+ number = integer | real.
number -> integer_dec : {number, str_of('$1'), '$1'}.
number -> integer_hex : {number, str_of('$1'), '$1'}.
number -> real : {number, str_of('$1'), '$1'}.

% module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .

module -> t_module ident t_semicolon import_list declaration_sequence t_begin_statement_sequence t_end ident t_dot : 
 {module, str_of('$1'), {'$2', '$4',  '$5',  '$6', '$8'}}.

t_begin_statement_sequence -> t_begin statement_sequence : '$2'.
t_begin_statement_sequence -> '$empty' : nil.

% ImportList = IMPORT import {"," import} ";".
import_list_rep -> import_list_rep t_comma import  : {import_list_rep, str_of('$1'), value_of('$1') ++ ['$3']}.
import_list_rep -> import : {import_list_rep, str_of('$1'), ['$1']}.
import_list -> t_import import_list_rep t_semicolon : {import_list, str_of('$1'), value_of('$2')}.
import_list -> '$empty' : nil.

% import = ident [":=" ident].
import -> ident t_assign ident: {import, str_of('$1'), {'$1', '$3'}}.
import -> ident : {import, str_of('$1'), '$1'}.

% DeclarationSequence = 
% [CONST {ConstDeclaration ";"}]
% [TYPE {TypeDeclaration ";"}]
% [VAR {VariableDeclaration ";"}] 
% {ProcedureDeclaration ";"}.

declaration_sequence -> ds_const_declaration ds_type_declaration ds_variable_declaration ds_procedure_declaration : 
  {declaration_sequence, {'$1', '$2', '$3', '$4'}}.

% [CONST {ConstDeclaration ";"}] 
ds_const_declaration_rep     -> ds_const_declaration_rep ds_const_declaration t_semicolon : {const_declaration_rep, str_of('$1'), value_of('$1') ++ ['$2']}.
ds_const_declaration_rep     -> const_declaration t_semicolon : {const_declaration_rep, str_of('$1'), ['$1']}.
ds_const_declaration -> t_const ds_const_declaration_rep : {const_declaration, str_of('$1'), '$2'}.
ds_const_declaration -> '$empty' : nil.

% [TYPE {TypeDeclaration ";"}] 
ds_type_declaration_rep      -> ds_type_declaration_rep type_declaration t_semicolon : {type_declaration_rep, str_of('$1'), value_of('$1') ++ ['$2']}.
ds_type_declaration_rep      -> type_declaration t_semicolon : {type_declaration_rep, str_of('$1'), ['$1']}.
ds_type_declaration -> t_type ds_type_declaration_rep : {type_declaration, str_of('$1'), '$2'}.
ds_type_declaration -> '$empty' : nil.

% [VAR {VariableDeclaration ";"}] 
ds_variable_declaration_rep  -> ds_variable_declaration_rep variable_declaration t_semicolon : {variable_declaration_rep, str_of('$1'), value_of('$1') ++ ['$2']}.
ds_variable_declaration_rep  -> variable_declaration t_semicolon : {variable_declaration_rep, str_of('$1'), ['$1']}.
ds_variable_declaration -> t_var ds_variable_declaration_rep : {variable_declaration, str_of('$1'), '$2'}.
ds_variable_declaration -> '$empty' : nil.

% {ProcedureDeclaration ";"}.
ds_procedure_declaration_rep -> ds_procedure_declaration_rep procedure_declaration t_semicolon : {procedure_declaration_rep, str_of('$1'), value_of('$1') ++ ['$2']}.
ds_procedure_declaration_rep -> procedure_declaration t_semicolon : {procedure_declaration_rep, str_of('$1'), ['$1']}.
ds_procedure_declaration -> ds_procedure_declaration_rep : {variable_declaration, str_of('$1'), '$1'}.
ds_procedure_declaration -> '$empty' : nil.

%+ ConstDeclaration = identdef "=" ConstExpression.
const_declaration -> identdef t_equ const_expression : {const_declaration, str_of('$1'), {'$1', '$3'}}.

% ConstExpression = expression.
const_expression -> expression : {const_expression, str_of('$1'), '$1'}.

% TypeDeclaration = identdef "=" StrucType.
type_declaration -> identdef t_equ struct_type : {type_declaration, str_of('$1'), {'$1', '$3'}}.

% StrucType = ArrayType | RecordType | PointerType | ProcedureType.
struct_type -> array_type : {struct_type, str_of('$1'), '$1'}.
struct_type -> record_type : {struct_type, str_of('$1'), '$1'}.
struct_type -> pointer_type : {struct_type, str_of('$1'), '$1'}.
struct_type -> procedure_type : {struct_type, str_of('$1'), '$1'}.

% ArrayType = ARRAY length {"," length} OF type.
% array_type = ARRAY array_type_rep OF type.
array_type -> t_array array_type_rep t_of type  : {array_type, str_of('$1'), {'$2', '$4'}}.
array_type_rep -> array_type_rep t_comma length : {array_type_rep, str_of('$1'), value_of('$1') ++ ['$3']}.
array_type_rep -> length : {array_type_rep, str_of('$1'), ['$1']}.

% length = ConstExpression.
length -> const_expression : {length, str_of('$1'), '$1'}.

% RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
record_type -> t_record record_type_part1 record_type_part2 t_end : 
  {record_type, {'$2', '$3'}}.

record_type_part1 -> t_lpar base_type t_rpar : '$2'.
record_type_part1 -> '$empty' : nil.
record_type_part2 -> field_list_sequence : '$1'.
record_type_part2 -> '$empty' : nil.

% BaseType = qualident.
base_type -> qualident : {base_type, str_of('$1'), '$1'}.

% FieldListSequence = FieldList {";" FieldList}.
% FieldListSequence = field_list_sequence_rep.
field_list_sequence -> field_list_sequence_rep : {field_list_sequence, str_of('$1'), '$1'}.
field_list_sequence_rep -> field_list_sequence_rep t_semicolon field_list: {field_list_sequence_rep, str_of('$1'), value_of('$1') ++ ['$3']}.
field_list_sequence_rep -> field_list : {field_list_sequence_rep, str_of('$1'), ['$1']}.

% FieldList = IdentList ":" type.
field_list -> ident_list t_colon type : {field_list, str_of('$1'), {'$1', '$3'}}.

% IdentList = identdef {"," identdef}.
ident_list -> ident_list_rep : '$1'.
ident_list_rep -> identdef t_comma ident_list_rep: {ident_list, str_of('$1'), ['$1'] ++ value_of('$3')}.
ident_list_rep -> identdef : {ident_list, str_of('$1'), ['$1']}.

% PointerType = POINTER TO type.
pointer_type -> t_pointer t_to type : {pointer_type, str_of('$1'), '$3'}.

% ProcedureType = PROCEDURE [FormalParameters].
procedure_type -> t_procedure formal_parameters: {procedure_type, str_of('$1'), '$2'}.
procedure_type -> t_procedure : {procedure_type, nil, nil}.

% FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
formal_parameters_qual_rep -> t_colon qualident : {formal_parameters, str_of('$1'), '$2'}.
formal_parameters_qual_rep -> '$empty' : nil.

formal_parameters_fps_rep2 -> formal_parameters_fps_rep2 t_semicolon fpsection: {fpsection, str_of('$1'), value_of('$1') ++ ['$3']}.
formal_parameters_fps_rep2 -> fpsection : {fpsection, str_of('$1'), ['$1']}.
formal_parameters_fps_rep2 -> '$empty' : nil.

formal_parameters_fps_rep -> t_lpar formal_parameters_fps_rep2 t_rpar : '$2'.
formal_parameters_fps_rep -> '$empty' : nil.

formal_parameters -> formal_parameters_fps_rep formal_parameters_qual_rep : {formal_parameters, {'$1', '$2'}}.

% FPSection = [VAR] ident {"," ident} ":" FormalType.
fpsection -> t_var fpsection_ident_rep t_colon formal_type : {fpsection, str_of('$1'), {not_var, '$2','$4'}}.
fpsection -> fpsection_ident_rep t_colon formal_type : {fpsection, str_of('$1'), {not_var, '$1','$3'}}.
fpsection_ident_rep -> fpsection_ident_rep t_comma ident : {fpsection_ident_rep, str_of('$1'), value_of('$1') ++ ['$3']}.
fpsection_ident_rep -> ident : {fpsection_ident_rep, str_of('$1'), ['$1']}.

% FormalType = {ARRAY OF} qualident.
formal_type -> formal_type_rep qualident : {formal_type, str_of('$2'), {'$1', '$2'}}.
formal_type_rep -> t_array t_of formal_type_rep : {array_of, '$3'}.
formal_type_rep -> '$empty' : nil.

% qualident = [ident "."] ident.
qualident_ident -> ident ident_t_dot : {'$1', '$2'}.
qualident_ident -> '$empty' : nil.
qualident_ident2 -> ident : '$1'.
ident_t_dot -> t_dot : '$1'.
qualident -> qualident_ident qualident_ident2 : {qualident, str_of('$1'), {'$1', '$2'}}.

% identdef = ident ["*"].
identdef -> ident t_mul : {identdef, str_of('$1'), value_of('$1')++"*"}.
identdef -> ident       : {identdef, str_of('$1'), value_of('$1')}.

% VariableDeclaration = IdentList ":" type.
variable_declaration -> ident_list t_colon type : {variable_declaration, str_of('$1'), {'$1', '$3'}}.

% type = qualident | StrucType.
type -> qualident : {type, str_of('$1'), '$1'}.
type -> struct_type : {type, str_of('$1'), '$1'}.

% ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
procedure_declaration -> procedure_heading t_semicolon procedure_body ident : 
  {procedure_declaration, str_of('$1'), {'$1', '$3', '$4'}}.

% ProcedureHeading = PROCEDURE identdef [FormalParameters].
procedure_heading -> t_procedure identdef formal_parameters : {procedure_heading, str_of('$1'), {'$2', '$3'}}.

% ProcedureBody = DeclarationSequence [BEGIN StatementSequence] [RETURN expression] END.
procedure_body -> declaration_sequence procedure_body_part1 procedure_body_part2 t_end : {'$1', '$2', '$3'}.
procedure_body_part1 -> t_begin statement_sequence : '$2'.
procedure_body_part1 -> '$empty' : nil.
procedure_body_part2 -> t_return expression : '$2'.
procedure_body_part2 -> '$empty' : nil.

%+ expression = SimpleExpression [relation SimpleExpression].
expression -> simple_expression relation simple_expression: {expression, str_of('$1'), {'$1', '$2', '$3'}}.
expression -> simple_expression : {expression, str_of('$1'), '$1'}.

%+ relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
relation -> t_equ : {relation, str_of('$1'), '$1'}.
relation -> t_sharp : {relation, str_of('$1'), '$1'}.
relation -> t_less : {relation, str_of('$1'), '$1'}.
relation -> t_lesseq : {relation, str_of('$1'), '$1'}.
relation -> t_more : {relation, str_of('$1'), '$1'}.
relation -> t_moreeq : {relation, str_of('$1'), '$1'}.
relation -> t_in : {relation, str_of('$1'), '$1'}.
relation -> t_is : {relation, str_of('$1'), '$1'}.

%+ SimpleExpression = ["+" | "-"] term {AddOperator term}.
simple_expression -> simple_expression_pre term simple_expression_rep : {simple_expression, str_of('$2'), {'$1', '$2', '$3'}}.

simple_expression_rep -> simple_expression_rep add_operator term : {'$1', '$2', '$3'}.
simple_expression_rep -> add_operator term : {'$1', '$2'}.
simple_expression_rep -> '$empty' : nil.

simple_expression_pre -> t_plus : plus.
simple_expression_pre -> t_minus : minus.
simple_expression_pre -> '$empty' : nil.

% AddOperator = "+" | "-" | OR.
add_operator -> t_plus : {add_operator, str_of('$1'), '$1'}.
add_operator -> t_minus : {add_operator, str_of('$1'), '$1'}.
add_operator -> t_or : {add_operator, str_of('$1'), '$1'}.

% term = factor {MulOperator factor}.
term_rep -> term_rep mul_operator factor: {term_rep, str_of('$2'), {'$1','$2','$3'}}.
term_rep -> factor : {term_rep, str_of('$1'), {'$1'}}.
term_rep -> '$empty' : nil.
term -> factor term_rep: {term, str_of('$1'), {'$1', '$2'}}.

% MulOperator = "*" | "/" | DIV | MOD | "&".
mul_operator -> t_mul : {mul_operator, str_of('$1'), '$1'}.
mul_operator -> t_divide : {mul_operator, str_of('$1'), '$1'}.
mul_operator -> t_div : {mul_operator, str_of('$1'), '$1'}.
mul_operator -> t_mod : {mul_operator, str_of('$1'), '$1'}.
mul_operator -> t_and : {mul_operator, str_of('$1'), '$1'}.

%+ factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
factor -> number : {factor, str_of('$1'), '$1'}.
factor -> string : {factor, str_of('$1'), '$1'}.
factor -> t_nil : {factor, str_of('$1'), '$1'}.
factor -> t_true : {factor, str_of('$1'), '$1'}.
factor -> t_false : {factor, str_of('$1'), '$1'}.
factor -> set : {factor, str_of('$1'), '$1'}.
factor -> designator actual_parameters: {factor, str_of('$1'), {'$1', '$2'}}.
factor -> designator : {factor, nil, '$1'}.
factor -> factor_expression : {factor, '$1'}.
factor -> t_tilda factor : {factor, str_of('$1'), {'$1', '$2'}}.

% "(" expression ")"
factor_expression -> t_lpar expression t_rpar : {factor_expression, str_of('$1'), {'$1', '$2', '$3'}}.
% WARNING! Заменено на ExpList иначе не понять чисто синтаксически
% factor_expression -> t_lpar exp_list t_rpar : {factor_expression, str_of('$1'), {'$1', '$2', '$3'}}.

% designator = qualident {selector}.
designator_rep -> designator_rep selector: {designator_rep, str_of('$1'), value_of('$1') ++ ['$2']}.
designator_rep -> selector: {designator_rep, str_of('$1'), ['$1']}.
designator_rep -> '$empty' : [].
designator -> qualident designator_rep : {designator, str_of('$1'), {'$1', value_of('$2')}}.

% selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
selector -> selector_t_dot selector_ident : {selector, str_of('$1'), {'$1', '$2'}}.
selector -> t_lbrack exp_list t_rbrack : {selector, str_of('$1'), {'$1', '$2', '$3'}}.
selector -> t_arrow : {selector, str_of('$1'), '$1'}.
selector -> selector_pars : '$1'.
selector_pars -> t_lpar qualident t_rpar : {selector, str_of('$1'), {'$1', '$2', '$3'}}.
selector_t_dot -> t_dot : '$1'.
selector_ident -> ident : '$1'.

% set = "{" [ element {"," element} ] "}".
set_rep -> set_rep t_comma element : {set_rep, str_of('$1'), value_of('$1')++['$3']}.
set_rep -> element : {set_rep, str_of('$1'), ['$1']}.
set_rep -> '$empty' : [].
set -> t_lbrace set_rep t_rbrace : {set_rep, str_of('$1'), '$2'}.

% element = expression [".." expression].
element -> expression t_ddot expression: {element, str_of('$1'), {'$1', '$3'}}.
element -> expression : {element, str_of('$1'), '$1'}.

% ExpList = expression {"," expression}.
% ExpList = exp_list_rep.
exp_list -> exp_list_rep : {exp_list, str_of('$1'), '$1'}.
exp_list_rep -> exp_list_rep t_comma expression: {exp_list_rep, str_of('$1'), value_of('$1') ++ ['$3']}.
exp_list_rep -> expression : {exp_list_rep, str_of('$1'), ['$1']}.

% ActualParameters = "(" [ExpList] ")" .
actual_parameters -> t_lpar exp_list t_rpar : {actual_parameters, str_of('$1'), '$2'}.
actual_parameters -> t_lpar t_rpar : {actual_parameters, str_of('$1'), nil}.

% statement = [assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement].
statement -> assignment       : {statement, str_of('$1'), '$1'}.
statement -> procedure_call   : {statement, str_of('$1'), '$1'}.
statement -> if_statement     : {statement, str_of('$1'), '$1'}.
statement -> case_statement   : {statement, str_of('$1'), '$1'}.
statement -> while_statement  : {statement, str_of('$1'), '$1'}.
statement -> repeat_statement : {statement, str_of('$1'), '$1'}.
statement -> for_statement    : {statement, str_of('$1'), '$1'}.
statement -> '$empty' : nil.

% assignment = designator ":=" expression.
assignment -> designator t_assign expression : {assignment, str_of('$1'), {'$1', '$3'}}.

% ProcedureCall = designator [ActualParameters].
% procedure_call -> designator procedure_call_parameters : {procedure_call, str_of('$1'), {'$1', '$2'}}.
procedure_call -> designator : {procedure_call, str_of('$1'), {'$1', nil}}.
% procedure_call_parameters -> actual_parameters : {procedure_call_parameters1, '$1'}.

% StatementSequence = statement {";" statement}.
% StatementSequence = statement_sequence_rep.
statement_sequence_rep -> statement_sequence_rep t_semicolon statement: {statement_sequence_rep, str_of('$1'), value_of('$1') ++ ['$3']}.
statement_sequence_rep -> statement : {statement_sequence_rep, str_of('$1'), ['$1']}.
statement_sequence -> statement_sequence_rep : {statement_sequence, str_of('$1'), value_of('$1')}.

% IfStatement = IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence} [ELSE StatementSequence] END.
% if_statement = IF expression THEN StatementSequence if_statement_rep [ELSE StatementSequence] END.
if_statement -> t_if expression t_then statement_sequence if_statement_rep t_else_statement_sequence t_end : 
  {if_statement, str_of('$1'), {'$2', '$4', '$5', '$6'}}.

t_else_statement_sequence -> t_else statement_sequence : '$2'.
t_else_statement_sequence -> '$empty' : nil.
if_statement_rep -> if_statement_rep u_elsif expression t_then statement_sequence :
  {if_statement_rep, str_of('$1'), value_of('$1') ++[{'$3', '$5'}]}.
if_statement_rep -> u_elsif expression t_then statement_sequence :
  {if_statement_rep, str_of('$1'), [{'$2', '$4'}]}.
if_statement_rep -> '$empty' : nil.

u_elsif -> t_elsif : '$1'.
u_elsif -> t_elseif : '$1'.

% CaseStatement = CASE expression OF case {"|" case} END.
% CaseStatement = CASE expression OF case_statement_rep END.
case_statement -> t_case expression t_of case_statement_rep t_end : {case_statement, str_of('$1'), {'$2', '$4'}}.

case_statement_rep -> case_statement_rep t_vline ntcase: {case_statement_rep, str_of('$1'), ['$1'] ++ ['$3']}.
case_statement_rep -> ntcase : {case_statement_rep, '$1', ['$1']}.

% case = [CaseLabelList ":" StatementSequence].
% ntcase -> '$empty' : 'Elixir.T':new({ntcase, nil, nil}).
ntcase -> case_label_list t_colon statement_sequence : {ntcase, '$1', {'$1', '$3'}}.
ntcase -> '$empty' : nil.

% +CaseLabelList = LabelRange {"," LabelRange}.
case_label_list_rep -> label_range t_comma case_label_list_rep : {case_label_list, str_of('$1'),[('$1')] ++ value_of('$3')}.
case_label_list_rep -> label_range : {case_label_list, str_of('$1'), [('$1')]}.
case_label_list -> case_label_list_rep : '$1'.

% LabelRange = label [".." label].
label_range -> label t_ddot label: {label_range, str_of('$1'), {'$1', '$3'}}.
label_range -> label : {label_range, str_of('$1'), '$1'}.

% label = integer | string | qualident.
label -> qualident : {label, str_of('$1'), '$1'}.
label -> string : {label, str_of('$1'), '$1'}.
label -> integer_dec : {label, str_of('$1'), '$1'}.
label -> integer_hex : {label, str_of('$1'), '$1'}.

% WhileStatement = WHILE expression DO StatementSequence {ELSIF expression DO StatementSequence} END.
% while_statement = WHILE expression DO StatementSequence while_statement_rep END.
while_statement -> t_while expression t_do statement_sequence while_statement_rep t_end : {while_statement, str_of('$1'), {'$2', '$3', '$4'}}.

while_statement_rep -> while_statement_rep u_elsif expression t_do statement_sequence : {while_statement_rep, str_of('$1'), value_of('$1') ++ [{'$3', '$5'}]}.
while_statement_rep -> u_elsif expression t_do statement_sequence : {while_statement_rep, str_of('$1'), [{'$2', '$4'}]}.
while_statement_rep -> '$empty' : nil.

% RepeatStatement = REPEAT StatementSequence UNTIL expression.
repeat_statement -> t_repeat statement_sequence t_until expression : 
 {repeat_statement, str_of('$1'), {'$2', '$4'}}.

% ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression] DO StatementSequence END.
for_statement -> t_for ident t_assign expression t_to expression t_by const_expression t_do statement_sequence t_end : 
  {for_statement, str_of('$1'), {'$2', '$4', '$6', '$8', '$10'}}.
for_statement -> t_for ident t_assign expression t_to expression t_do statement_sequence t_end : 
  {for_statement, str_of('$1'), {'$2', '$4', '$6',  nil, '$8'}}.


Erlang code.

-define(MType, '__struct__').

% list_tail({_, List}) -> List.
str_of(Obj) when is_tuple(Obj) -> tstr_of(Obj);
str_of(Obj) when is_map(Obj) -> mstr_of(Obj);
str_of(_) -> nil.

value_of(Obj) when is_tuple(Obj) -> tvalue_of(Obj);
value_of(Obj) when is_map(Obj) -> mvalue_of(Obj);
value_of(_) -> nil.

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

% hex_or_letter({letter, Str, "A"}) -> {hex_digit, Str, "A"};
% hex_or_letter({letter, Str, "B"}) -> {hex_digit, Str, "B"};
% hex_or_letter({letter, Str, "C"}) -> {hex_digit, Str, "C"};
% hex_or_letter({letter, Str, "D"}) -> {hex_digit, Str, "D"};
% hex_or_letter({letter, Str, "E"}) -> {hex_digit, Str, "E"};
% hex_or_letter({letter, Str, "F"}) -> {hex_digit, Str, "F"};
% hex_or_letter(Other) -> Other.
