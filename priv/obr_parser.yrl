% Для описания синтаксиса Оберона-2 используются Расширенные Бэкуса-Наура Формы (РБНФ). 
% Варианты разделяются знаком |. 
% Квадратные скобки [ и ] означают необязательность записанного внутри них выражения, 
% а фигурные скобки { и } означают его повторение (возможно 0 раз). 
% Нетерминальные символы начинаются с заглавной буквы (например, Оператор). 
% Терминальные символы или начинаются малой буквой (например, идент), 
% или записываются целиком заглавными буквами (например, BEGIN), или заключаются в кавычки (например, ":=").

Nonterminals 
module
.

Terminals 
ident integer real.

Rootsymbol module.

module -> expr : '$1'.

% то, что уже есть благодаря лексеру
% ident  =  letter {letter | digit}. - ident уже идет из лексера
% letter =  "A" .. "Z" | "a" .. "z". 
% digit  =  "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9". 
% integer  =  digit {digit} | digit {hexDigit} "H". 
% real  =  digit {digit} "." {digit} [ScaleFactor]. 
% hexDigit  =  digit | "A" | "B" | "C" | "D" | "E" | "F". 
% CharConstant  =  '"' character '"' | digit {hexDigit} "X". 
% string  =  '"' {character} '"'. 

% number  =  integer | real. 
number -> integer : '$1'.
number -> real : '$1'.

% module  =  MODULE ident ";"  [ImportList] DeclarationSequence 
%     [BEGIN StatementSequence] END ident ".". 
% ImportList  =  IMPORT import {"," import} ";". 
% import  =  ident [":=" ident]. 
% DeclarationSequence  =  {CONST {ConstantDeclaration ";"} | 
%     TYPE {TypeDeclaration ";"} | VAR {VariableDeclaration ";"}} 
%     {ProcedureDeclaration ";" | ForwardDeclaration ";"}. 
% ConstantDeclaration  =  identdef "=" ConstExpression. 
% identdef  =  ident ["*"]. 
% ConstExpression  =  expression. 
% expression  =  SimpleExpression [relation SimpleExpression]. 
% SimpleExpression  =  ["+"|"-"] term {AddOperator term}. 
% term  =  factor {MulOperator factor}. 
% factor  =  number | CharConstant | string | NIL | set | 
%     designator [ActualParameters] | "(" expression ")" | "~" factor. 
% ScaleFactor  =  ("E" | "D") ["+" | "-"] digit {digit}. 
% set  =  "{" [element {"," element}] "}". 
% element  =  expression [".." expression]. 
% designator  =  qualident {"." ident | "[" ExpList "]" | "(" qualident ")" | "^" }. 
% ExpList  =  expression {"," expression}. 
% ActualParameters  =  "(" [ExpList] ")". 
% MulOperator  =  "*" | "/" | DIV | MOD | "&". 
% AddOperator  =  "+" | "-" | OR. 
% relation  =  "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS. 
% TypeDeclaration  =  identdef "=" type. 
% type  =  qualident | ArrayType | RecordType | PointerType | ProcedureType. 
% qualident  =  [ident "."] ident. 
% ArrayType  =  ARRAY length {"," length} OF type. 
% length  =  ConstExpression. 
% RecordType  =  RECORD ["(" BaseType ")"] FieldListSequence END. 
% BaseType  =  qualident. 
% FieldListSequence  =  FieldList {";" FieldList}. 
% FieldList  =  [IdentList ":" type]. 
% IdentList  =  identdef {"," identdef}. 
% PointerType  =  POINTER TO type. 
% ProcedureType  =  PROCEDURE [FormalParameters]. 
% VariableDeclaration  =  IdentList ":" type. 
% ProcedureDeclaration  =  ProcedureHeading ";" ProcedureBody ident. 
% ProcedureHeading  =  PROCEDURE ["*"] identdef [FormalParameters]. 
% FormalParameters  =  "(" [FPSection {";" FPSection}] ")" [":" qualident]. 
% FPSection  =  [VAR] ident {"," ident} ":" FormalType. 
% FormalType  =  {ARRAY OF} (qualident | ProcedureType). 
% ProcedureBody  =  DeclarationSequence [BEGIN StatementSequence] END. 
% ForwardDeclaration  =  PROCEDURE "^" ident ["*"] [FormalParameters]. 
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

value_of(Token) ->
    element(3, Token).