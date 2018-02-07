% :leex.file('obr.xrl')
% c("obr.erl")
% :obr.string('123')

Definitions.

LETTER     = [A-Za-z]
DIGIT      = [0-9]
IDENT      = {LETTER}({LETTER}|{DIGIT})*
INTHEXWR   = ([0-9A-F])
INTHEX     = ([0-9A-F]+H)
INTDEC     = {DIGIT}+
INTEGER    = {INTHEX}|{INTDEC}
INTEGER2   = {INTHEXWR}+
REAL       = [0-9]+\.[0-9]+([E|D][-+]?[0-9]+)?
STRING1    = "([^"|^\n|^\r])*"
STRING2    = '([^'|^\n|^\r])*'
STRING     = {STRING1}|{STRING2}
CHARACTER  = ([0-9A-F]+X)

DOT        = \.
LBRACK     = \[
ARROW      = \^

DELIM      = [\s\t\n\r]
WS         = {DELIM}+


Rules.

{IDENT}     : {token, {ident,  TokenLine, id_validate(TokenChars, TokenLine)}}.
{INTEGER}   : {token, {integer, TokenLine, int_validate(TokenChars, TokenLine)}}.
{INTEGER2}  : {token, {integer, TokenLine, intwr_validate(TokenChars, TokenLine)}}.
{REAL}      : {token, {real, TokenLine, TokenChars}}.
{STRING}    : {token, {string, TokenLine, str_validate(TokenChars, TokenLine)}}.
{CHARACTER} : {token, {character, TokenLine, TokenChars}}.

{DOT}       : {token, {dot, TokenLine, TokenChars}}.
{LBRACK}    : {token, {lbrack, TokenLine, TokenChars}}.
{ARROW}     : {token, {arrow, TokenLine, TokenChars}}.

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
int_validate_del_zero([$0|Chars]) -> int_validate_del_zero(Chars);
int_validate_del_zero(Chars) -> Chars.

intwr_validate(Chars, Line) ->
  io:format("WARNING: A hexadecimal literal hasn't a trailing H. ~w: ~s -> ~s~n", [Line, Chars, Chars ++ "H"]),
  int_validate(Chars ++ "H", Line).

str_validate(Chars, _Line) -> Chars.
