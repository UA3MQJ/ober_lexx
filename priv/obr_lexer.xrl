% :leex.file('obr.xrl')
% c("obr.erl")
% :obr.string('123')

Definitions.

DELIM      = [\s\t\n\r]
WS         = {DELIM}+

ARRAY      = ARRAY
BEGIN      = BEGIN
BY         = BY
CASE       = CASE
CONST      = CONST
DIV        = DIV
DO         = DO
ELSE       = ELSE
ELSEIF     = ELSEIF
ELSIF      = ELSIF
END        = END
FALSE      = FALSE
FOR        = FOR
IF         = IF
IMPORT     = IMPORT
IN         = IN
IS         = IS
MODULE     = MODULE
MOD        = MOD
NIL        = NIL
OF         = OF
OR         = OR
POINTER    = POINTER
PROCEDURE  = PROCEDURE
RECORD     = RECORD
REPEAT     = REPEAT
RETURN     = RETURN
TO         = TO
TRUE       = TRUE
TYPE       = TYPE
VAR        = VAR
WHILE      = WHILE
THEN       = THEN
UNTIL      = UNTIL

% ABS        = ABS
% ASR        = ASR
% ASSERT     = ASSERT
% BOOLEAN    = BOOLEAN
% BYTE       = BYTE
% CHAR       = CHAR
% CHR        = CHR
% DEC        = DEC
% EXCL       = EXCL
% FLOOR      = FLOOR
% FLT        = FLT
% INCL       = INCL
% INC        = INC
% TINTEGER   = INTEGER
% LEN        = LEN
% LSL        = LSL
% NEW        = NEW
% ODD        = ODD
% ORD        = ORD
% PACK       = PACK
% TREAL      = REAL
% ROR        = ROR
% SET        = SET
% UNPK       = UNPK

PLUS       = \+
MINUS      = \-
MUL        = \*
DIVIDE     = /
TILDA      = ~
AND        = &
DDOT       = \.\.
DOT        = \.
COMMA      = ,
SEMICOLON  = ;
VLINE      = \|
LPAR       = \(
RPAR       = \)
LBRACK     = \[
RBRACK     = \]
LBRACE     = \{
RBRACE     = \}
ASSIGN     = \:\=
COLON      = \:
ARROW      = \^
EQU        = \=
SHARP      = \#
LESSEQ     = \<\=
MOREEQ     = \>\=
LESS       = \<
MORE       = \>


LETTER     = [A-Za-z]
DIGIT      = [0-9]
IDENT      = {LETTER}({LETTER}|{DIGIT})*
INTHEXWR   = ([0-9A-F])
INTHEX     = ([0-9A-F]+H)
INTDEC     = {DIGIT}+
INT        = {INTHEX}|{INTDEC}
INT2       = {INTHEXWR}+
REAL       = [0-9]+\.[0-9]+([E|D][-+]?[0-9]+)?
STRING1    = "([^"|^\n|^\r])*"
STRING2    = '([^'|^\n|^\r])*'
STRING     = {STRING1}|{STRING2}
CHARACTER  = ([0-9A-F]+X)


Rules.

{ARRAY}     : {token, {t_array, TokenLine, TokenChars}}.
{BEGIN}     : {token, {t_begin, TokenLine, TokenChars}}.
{BY}        : {token, {t_by, TokenLine, TokenChars}}.
{CASE}      : {token, {t_case, TokenLine, TokenChars}}.
{CONST}     : {token, {t_const, TokenLine, TokenChars}}.
{DIV}       : {token, {t_div, TokenLine, TokenChars}}.
{DO}        : {token, {t_do, TokenLine, TokenChars}}.
{ELSE}      : {token, {t_else, TokenLine, TokenChars}}.
{ELSEIF}    : {token, {t_elseif, TokenLine, TokenChars}}.
{ELSIF}     : {token, {t_elsif, TokenLine, TokenChars}}.
{END}       : {token, {t_end, TokenLine, TokenChars}}.
{FALSE}     : {token, {t_false, TokenLine, TokenChars}}.
{FOR}       : {token, {t_for, TokenLine, TokenChars}}.
{IF}        : {token, {t_if, TokenLine, TokenChars}}.
{IMPORT}    : {token, {t_import, TokenLine, TokenChars}}.
{IN}        : {token, {t_in, TokenLine, TokenChars}}.
{IS}        : {token, {t_is, TokenLine, TokenChars}}.
{MODULE}    : {token, {t_module, TokenLine, TokenChars}}.
{MOD}       : {token, {t_mod, TokenLine, TokenChars}}.
{NIL}       : {token, {t_nil, TokenLine, TokenChars}}.
{OF}        : {token, {t_of, TokenLine, TokenChars}}.
{OR}        : {token, {t_or, TokenLine, TokenChars}}.
{POINTER}   : {token, {t_pointer, TokenLine, TokenChars}}.
{PROCEDURE} : {token, {t_procedure, TokenLine, TokenChars}}.
{RECORD}    : {token, {t_record, TokenLine, TokenChars}}.
{REPEAT}    : {token, {t_repeat, TokenLine, TokenChars}}.
{RETURN}    : {token, {t_return, TokenLine, TokenChars}}.
{TO}        : {token, {t_to, TokenLine, TokenChars}}.
{TRUE}      : {token, {t_true, TokenLine, TokenChars}}.
{TYPE}      : {token, {t_type, TokenLine, TokenChars}}.
{VAR}       : {token, {t_var, TokenLine, TokenChars}}.
{WHILE}     : {token, {t_while, TokenLine, TokenChars}}.
{THEN}      : {token, {t_then, TokenLine, TokenChars}}.
{UNTIL}     : {token, {t_until, TokenLine, TokenChars}}.

% {ABS}       : {token, {t_abs, TokenLine, TokenChars}}.
% {ASR}       : {token, {t_asr, TokenLine, TokenChars}}.
% {ASSERT}    : {token, {t_assert, TokenLine, TokenChars}}.
% {BOOLEAN}   : {token, {t_boolean, TokenLine, TokenChars}}.
% {BYTE}      : {token, {t_byte, TokenLine, TokenChars}}.
% {CHAR}      : {token, {t_char, TokenLine, TokenChars}}.
% {CHR}       : {token, {t_chr, TokenLine, TokenChars}}.
% {DEC}       : {token, {t_dec, TokenLine, TokenChars}}.
% {EXCL}      : {token, {t_excl, TokenLine, TokenChars}}.
% {FLOOR}     : {token, {t_floor, TokenLine, TokenChars}}.
% {FLT}       : {token, {t_flt, TokenLine, TokenChars}}.
% {INCL}      : {token, {t_incl, TokenLine, TokenChars}}.
% {INC}       : {token, {t_inc, TokenLine, TokenChars}}.
% {TINTEGER}  : {token, {t_integer, TokenLine, TokenChars}}.
% {LEN}       : {token, {t_len, TokenLine, TokenChars}}.
% {LSL}       : {token, {t_lsl, TokenLine, TokenChars}}.
% {NEW}       : {token, {t_new, TokenLine, TokenChars}}.
% {ODD}       : {token, {t_odd, TokenLine, TokenChars}}.
% {ORD}       : {token, {t_ord, TokenLine, TokenChars}}.
% {PACK}      : {token, {t_pack, TokenLine, TokenChars}}.
% {TREAL}     : {token, {t_real, TokenLine, TokenChars}}.
% {ROR}       : {token, {t_ror, TokenLine, TokenChars}}.
% {SET}       : {token, {t_set, TokenLine, TokenChars}}.
% {UNPK}      : {token, {t_unpk, TokenLine, TokenChars}}.

{PLUS}      : {token, {t_plus, TokenLine, TokenChars}}.
{MINUS}     : {token, {t_minus, TokenLine, TokenChars}}.
{MUL}       : {token, {t_mul, TokenLine, TokenChars}}.
{DIVIDE}    : {token, {t_divide, TokenLine, TokenChars}}.
{TILDA}     : {token, {t_tilda, TokenLine, TokenChars}}.
{AND}       : {token, {t_and, TokenLine, TokenChars}}.
{DDOT}      : {token, {t_ddot, TokenLine, TokenChars}}.
{DOT}       : {token, {t_dot, TokenLine, TokenChars}}.
{COMMA}     : {token, {t_comma, TokenLine, TokenChars}}.
{SEMICOLON} : {token, {t_semicolon, TokenLine, TokenChars}}.
{VLINE}     : {token, {t_vline, TokenLine, TokenChars}}.
{LPAR}      : {token, {t_lpar, TokenLine, TokenChars}}.
{RPAR}      : {token, {t_rpar, TokenLine, TokenChars}}.
{LBRACK}    : {token, {t_lbrack, TokenLine, TokenChars}}.
{RBRACK}    : {token, {t_rbrack, TokenLine, TokenChars}}.
{LBRACE}    : {token, {t_lbrace, TokenLine, TokenChars}}.
{RBRACE}    : {token, {t_rbrace, TokenLine, TokenChars}}.
{ASSIGN}    : {token, {t_assign, TokenLine, TokenChars}}.
{COLON}     : {token, {t_colon, TokenLine, TokenChars}}.
{ARROW}     : {token, {t_arrow, TokenLine, TokenChars}}.
{EQU}       : {token, {t_equ, TokenLine, TokenChars}}.
{SHARP}     : {token, {t_sharp, TokenLine, TokenChars}}.
{LESSEQ}    : {token, {t_lesseq, TokenLine, TokenChars}}.
{MOREEQ}    : {token, {t_moreeq, TokenLine, TokenChars}}.
{LESS}      : {token, {t_less, TokenLine, TokenChars}}.
{MORE}      : {token, {t_more, TokenLine, TokenChars}}.


{IDENT}     : {token, {ident,  TokenLine, id_validate(TokenChars, TokenLine)}}.
{INT}       : {token, {integer, TokenLine, int_validate(TokenChars, TokenLine)}}.
{INT2}      : {token, {integer, TokenLine, intwr_validate(TokenChars, TokenLine)}}.
{REAL}      : {token, {real, TokenLine, TokenChars}}.
{STRING}    : {token, {string, TokenLine, str_validate(TokenChars, TokenLine)}}.
{CHARACTER} : {token, {character, TokenLine, TokenChars}}.

{WS}        : skip_token.

Erlang code.

id_validate(Chars, Line) ->
  case length(Chars) > 40 of
    true ->
      io:format("WARNING: Identifier too long. ~w: ~s -> ~s~n", [Line, Chars, lists:sublist(Chars, 40)]),
      lists:sublist(Chars, 40);
    _Else ->
      Chars
  end.

int_validate(Chars, _Line) -> int_validate_del_zero(Chars).
int_validate_del_zero([$0] = Chars) -> Chars;
int_validate_del_zero([$0|Chars]) -> int_validate_del_zero(Chars);
int_validate_del_zero(Chars) -> Chars.

intwr_validate(Chars, Line) ->
  io:format("WARNING: A hexadecimal literal hasn't a trailing H. ~w: ~s -> ~s~n", [Line, Chars, Chars ++ "H"]),
  int_validate(Chars ++ "H", Line).

str_validate(Chars, _Line) -> Chars.
