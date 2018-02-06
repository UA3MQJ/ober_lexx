% :leex.file('obr.xrl')
% c("obr.erl")
% :obr.string('123')

Definitions.

DELIM     = [\s\t\n\r]
WS        = {DELIM}+
LETTER    = [A-Za-z]
DIGIT     = [0-9]
INTHEXWR  = ([0-9A-F])
INTHEX		= ([0-9A-F]+H)
INTDEC    = {DIGIT}+
INT       = {INTHEX}|{INTDEC}
INT2      = {INTHEXWR}+
STR       = \'([^'\n]|\'\')+\'
ID        = {LETTER}({LETTER}|{DIGIT}|_)*

Rules.

{WS}    : skip_token.
{ID}    : {token, {id,  TokenLine, id_validate(TokenChars, TokenLine)}}.
{INT}   : {token, {int, TokenLine, int_validate(TokenChars, TokenLine)}}.
{INT2}  : {token, {int, TokenLine, intwr_validate(TokenChars, TokenLine)}}.
{STR}   : {token, {str, TokenLine, str_validate(TokenChars, TokenLine)}}.

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
int_validate_del_zero([$0|Chars]) -> int_validate_del_zero(Chars);
int_validate_del_zero(Chars) -> Chars.

intwr_validate(Chars, Line) ->
  io:format("WARNING: hex number not H at end. ~w: ~s -> ~s~n", [Line, Chars, Chars ++ "H"]),
  int_validate(Chars ++ "H", Line).

str_validate(Chars, _Line) -> Chars.