defmodule OberYaccTest do
  use ExUnit.Case

  require Logger

  # mix test --only yacc
  @tag yacc: true

  test "the truth" do
    time1 = :os.system_time(:millisecond)
    {:ok, _} = :leex.file(~c"./priv/obr_lexer.xrl", [verbose: true])
    time2 = :os.system_time(:millisecond)
    {:ok, :obr_lexer} = :c.c(~c"./priv/obr_lexer.erl")
    time3 = :os.system_time(:millisecond)

    Logger.debug "Leex - generate erl time = #{time2 - time1} ms"
    Logger.debug "Compile erl time = #{time3 - time2} ms"


    time1 = :os.system_time(:millisecond)
    {:ok, _} = :yecc.file(~c"./priv/obr_parser.yrl", [verbose: true])
    time2 = :os.system_time(:millisecond)
    {:ok, :obr_parser} = :c.c(~c"./priv/obr_parser.erl")
    time3 = :os.system_time(:millisecond)

    Logger.debug "Yecc - generate erl time = #{time2 - time1} ms"
    Logger.debug "Compile erl time = #{time3 - time2} ms"

    # # number
    # {:ok, tokens, _} = :obr_lexer.string('123')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('123.01')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('123.')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # importlist
    # {:ok, tokens, _} = :obr_lexer.string('IMPORT system;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('IMPORT system:=SYS;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('IMPORT system:=SYS, crt;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # qualident
    # {:ok, tokens, _} = :obr_lexer.string('ident1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('ident2.name')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # identdef
    # {:ok, tokens, _} = :obr_lexer.string('ident1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('ident_num1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('_ident_num1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('ident1*')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # factor
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('10')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('0')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('010')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('01.5')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('"string"')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('NIL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('TRUE')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('FALSE')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # set
    # {:ok, tokens, _} = :obr_lexer.string('{}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('{1}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('{1+1}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('{1..3}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('{1, 2}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('{1, 2..4}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('{1,3,5,6}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('{1,3..6}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('~1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('(1+2)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('arr.arr[i](IntPoint).x')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('par(Ext1).r')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)


    # # term
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1 * 2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1*2/3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # statement
    # {:ok, tokens, _} = :obr_lexer.string('a := 1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)


    # # expression
    # {:ok, tokens, _} = :obr_lexer.string('-100')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('-1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('+1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1+2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('-1+2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1+2+33')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('"aa"+"bbb"')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('22X')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('22X+33X')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('NIL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1*2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('3+1*2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('x IS NIL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1 = 3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1 IN {1,2,3}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # constantdeclaration
    # {:ok, tokens, _} = :obr_lexer.string('aaaa1=1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('aaaa1=1+1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # explist
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1,2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1,2,3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1,2+2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('xxx.cmp(asd, fds)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # actualparameters
    # {:ok, tokens, _} = :obr_lexer.string('()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('(1)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('(1,2)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # designator
    # {:ok, tokens, _} = :obr_lexer.string('tree')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('tree.left')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('tree.left.right')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('tree.left.right.up.down')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('tree[1]')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('tree[1][2][3]')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # TODO
    # {:ok, tokens, _} = :obr_lexer.string('tree(name).subnode')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('par.p(name)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('tree^^^')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('a()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # TODO
    # {:ok, tokens, _} = :obr_lexer.string('tree(name).subnode(name)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('w[3]')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('w[3].name[i]')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('tree[1][2]^[3]')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # identlist
    # {:ok, tokens, _} = :obr_lexer.string('day')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('day, month, year')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # fieldlist
    # {:ok, tokens, _} = :obr_lexer.string('day, month, year : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # fieldlistsequence
    # {:ok, tokens, _} = :obr_lexer.string('name, firstname: INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('name, firstname: INTEGER; a, b: WORD')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('name, firstname: INTEGER;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('name, firstname: INTEGER; a, b: WORD;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)


    # # type
    # {:ok, tokens, _} = :obr_lexer.string('blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY N0, N1, N2 OF blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('RECORD day, month, year: INTEGER \nEND')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY 10 OF blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY num OF blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # formaltype
    # {:ok, tokens, _} = :obr_lexer.string('XXX')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY OF XXX')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY OF ARRAY OF XXX')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # fpsection
    # # [VAR] ident {"," ident} ":" FormalType.
    # {:ok, tokens, _} = :obr_lexer.string('sukabl : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('sukabl, yat : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('VAR sukabl : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('VAR sukabl, yat : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # formalparameters
    # # FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
    # {:ok, tokens, _} = :obr_lexer.string('()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('() : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('(VAR x: INTEGER)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('(VAR x: INTEGER) : REAL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('(VAR x, y, z: INTEGER)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('(VAR x, y, z: INTEGER; VAR a,b:REAL; n: CHAR)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # type продолжение
    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE (a:integer)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE (VAR a:integer)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE (a:integer):real')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE (VAR a:integer):real')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # type - RecordType
    # {:ok, tokens, _} = :obr_lexer.string('RECORD END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer; b:real END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer; b:real; END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('RECORD (Node) END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('RECORD (Node) a:integer; b:real; END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)


    # # type - pointer to
    # {:ok, tokens, _} = :obr_lexer.string('POINTER TO aaa')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # VariableDeclaration
    # {:ok, tokens, _} = :obr_lexer.string('a, bb, ccc, ddddd:INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # procedureheading
    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1(a:INTEGER)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1(a:INTEGER) : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1() : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)


    # # length
    # {:ok, tokens, _} = :obr_lexer.string('1+2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # typedeclaration
    # {:ok, tokens, _} = :obr_lexer.string('Table       =  ARRAY N OF REAL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string(' Tree        =  POINTER TO Node')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('Function*   =  PROCEDURE (x: INTEGER): INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('  CenterNode  =  RECORD
    #                name: ARRAY 32 OF CHAR;
    #                subnode: Tree
    #              END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # constantdeclaration
    # {:ok, tokens, _} = :obr_lexer.string('N      =  100')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('limit  =  2*N -1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('all    =  {0 .. WordSize-1}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # label
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('"abc"')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('x')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # labelrange
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1..2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # caselabellist
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1, 2, 3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('1..2, 5..6')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # assignment
    # {:ok, tokens, _} = :obr_lexer.string('x := 123')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # procedurecall
    # {:ok, tokens, _} = :obr_lexer.string('procedurename')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('procedurename()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(1+1)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(2)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(xx)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(xx.yy.zz)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(1,2,3)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # statementsequence
    # {:ok, tokens, _} = :obr_lexer.string('a := 1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('a := 1; b:=2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # case
    # {:ok, tokens, _} = :obr_lexer.string('0 : x := x + y')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('55 : x := x + y')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('55..66 : x := x + y')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # ntcaselist
    # {:ok, tokens, _} = :obr_lexer.string('22 : x := x + y | 33 : x := x - y ')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # casestatement
    # {:ok, tokens, _} = :obr_lexer.string('CASE x OF 22 : x := x + y | 33 : x := x - y END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # str = '''
    # CASE i OF
    #   0: n := 0
    # | 2: n := 1
    # | 4: n := 2
    # | 6: n := 3
    # | 8: n := 4
    # |10: n := 5
    # END
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # IfStatement
    # {:ok, tokens, _} = :obr_lexer.string('IF x>1 THEN a:=0 END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('IF x>1 THEN a:=0 ELSIF x<1 THEN a:=-1 END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('IF x>1 THEN a:=0 ELSIF x<1 THEN a:=-1 ELSIF x=1 THEN a:=1 END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # {:ok, tokens, _} = :obr_lexer.string('IF x>1 THEN a:=0 ELSIF x<1 THEN a:=-1 ELSIF x=1 THEN a:=1 ELSE a:= 555 END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # repeatstatement
    # {:ok, tokens, _} = :obr_lexer.string('REPEAT a:=a+1 UNTIL a<100')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # whilestatement
    # str = '''
    # WHILE j > 0 DO
    #     j := j DIV 2; i := i+1
    # END
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # TODO
    # str = '''
    #     WHILE (j >= 0) & (cmp.compare(cmp, a, arr[j]) < 0) DO
    #         arr[j + 1] := arr[j];
    #         DEC(j)
    #     END
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # str = '''
    # WHILE m > n DO m := m - n
    # ELSIF n > m DO n := n - m
    # END
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # str = '''
    # WHILE m > n DO m := m - n
    # ELSIF n > m DO n := n - m
    # ELSIF n = m DO n := n * m
    # END
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # forstatement
    # str = 'FOR v := beg TO end DO S END'
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # str = 'FOR v := beg TO end BY inc DO S END'
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # proceduredeclaration
    # str = '''
    #     PROCEDURE ReadInt(VAR x: INTEGER);
    #             VAR i : INTEGER; ch: CHAR;
    #     BEGIN i := 0; Read(ch);
    #             WHILE ("0" <= ch) & (ch <= "9") DO
    #                     i := 10*i + (ORD(ch)-ORD("0")); Read(ch)
    #             END ;
    #             x := i
    #     END ReadInt
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # # module
    # str = '''
    #     MODULE ASCII;

    #     END ASCII.
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # str = '''
    #     MODULE ASCII;
    #     IMPORT system;
    #     END ASCII.
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # str = '''
    #     system.x.y(a)
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # {:ok, res} = :obr_parser.parse(tokens)
    # IO.inspect(res)

    # test_file("./priv/mods/examples/BasicRegs1.Mod") # TODO THEN END
    # test_file("./priv/mods/examples/Best40Test.Mod") # TODO CONST
    # test_file("./priv/mods/examples/Kubik.Mod") # TODO {:error, {43, :obr_lexer, {:illegal, '"|'}}, 43}

    test_file("./test/vostok-tests/Add.mod")
    test_file("./test/vostok-tests/Array.mod")
    test_file("./test/vostok-tests/Bits.mod")
    test_file("./test/vostok-tests/Bool.mod")
    test_file("./test/vostok-tests/BubbleSortTest.mod")
    test_file("./test/vostok-tests/Byte.mod")
    test_file("./test/vostok-tests/Case.mod")
    test_file("./test/vostok-tests/Char.mod")
    test_file("./test/vostok-tests/Chars.mod")
    test_file("./test/vostok-tests/Const.mod")
    test_file("./test/vostok-tests/Copy.mod")
    test_file("./test/vostok-tests/Div.mod")
    test_file("./test/vostok-tests/For.mod")
    test_file("./test/vostok-tests/Integers.mod")
    test_file("./test/vostok-tests/LinkedList.mod")
    test_file("./test/vostok-tests/MathTest.mod")
    test_file("./test/vostok-tests/Mult.mod")
    test_file("./test/vostok-tests/OopInsertSort.mod")
    test_file("./test/vostok-tests/Pointers.mod")
    test_file("./test/vostok-tests/ProcType.mod")
    test_file("./test/vostok-tests/PtrLoop.mod")
    test_file("./test/vostok-tests/Queens.mod")
    test_file("./test/vostok-tests/QuickSort.mod")
    test_file("./test/vostok-tests/Rand.mod")
    test_file("./test/vostok-tests/Real.mod")
    test_file("./test/vostok-tests/RecordExt.mod")
    test_file("./test/vostok-tests/Record.mod")
    test_file("./test/vostok-tests/Repeat.mod")
    test_file("./test/vostok-tests/Return.mod")
    test_file("./test/vostok-tests/Set.mod")
    test_file("./test/vostok-tests/String.mod")
    test_file("./test/vostok-tests/Test.mod")
    test_file("./test/vostok-tests/WeakLink.mod")
    test_file("./test/vostok-tests/While.mod")

  end

  def test_file(file) do
    # read binary
    {:ok, binary} = File.read(file)
    # bynary -> string
    str = OberLexx.Utils.raw_binary_to_string(binary)
    # string - remove comments -> string
    # IO.puts(str)
    clean_str = OberLexx.Utils.binary_remove_comments(str)
    # IO.puts(clean_str)
    # string -> charlist
    charlist = to_charlist(clean_str)
    # IO.inspect(charlist)
    {:ok, tokens, _} = :obr_lexer.string(charlist)
    # IO.inspect(tokens)
    case :obr_parser.parse(tokens) do
      {:ok, _res} ->
        # IO.inspect(res)
        Logger.info "#{file} - ok"
      error ->
        Logger.info "#{file} - error"
        Logger.debug "#{inspect error}"
    end
  end
end
