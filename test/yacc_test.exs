defmodule OberYaccTest do
  use ExUnit.Case

  require Logger

  # mix test --only yacc
  @tag yacc: true

  test "the truth" do

    time1 = :os.system_time(:millisecond)
    {:ok, _} = :leex.file('./priv/obr_lexer.xrl', [verbose: true])
    time2 = :os.system_time(:millisecond)
    {:ok, :obr_lexer} = :c.c('./priv/obr_lexer.erl')
    time3 = :os.system_time(:millisecond)

    Logger.debug "Leex - generate erl time = #{time2 - time1} ms"
    Logger.debug "Compile erl time = #{time3 - time2} ms"


    time1 = :os.system_time(:millisecond)
    {:ok, _} = :yecc.file('./priv/obr_parser.yrl', [verbose: true])
    time2 = :os.system_time(:millisecond)
    {:ok, :obr_parser} = :c.c('./priv/obr_parser.erl')
    time3 = :os.system_time(:millisecond)

    Logger.debug "Yecc - generate erl time = #{time2 - time1} ms"
    Logger.debug "Compile erl time = #{time3 - time2} ms"

    # # number
    # {:ok, tokens, _} = :obr_lexer.string('123')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('123.01')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # importlist
    # {:ok, tokens, _} = :obr_lexer.string('IMPORT system;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('IMPORT system:=SYS;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('IMPORT system:=SYS, crt;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # qualident
    # {:ok, tokens, _} = :obr_lexer.string('ident1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ident2.name')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ident2.name.name')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # identdef
    # {:ok, tokens, _} = :obr_lexer.string('ident1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ident1*')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # set
    # {:ok, tokens, _} = :obr_lexer.string('{1,3,5,6}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('{1,3..6}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('{}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('{1}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('{1, 2}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('{1, 2..4}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"


    # # expression
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('+1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('-1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1+2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1+2+33')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('"aa"+"bbb"')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('22X+33X')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('NIL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1*2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('3+1*2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('3+1*2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('x')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('x IS NIL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # TODO     {:ok, tokens, _} = :obr_lexer.string('1 = 0')

    # {:ok, tokens, _} = :obr_lexer.string('1 = 3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1 IN {1,2,3}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # constantdeclaration
    # {:ok, tokens, _} = :obr_lexer.string('aaaa1=1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('aaaa1=1+1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # explist
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1,2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1,2,3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1,2+2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # actualparameters
    # {:ok, tokens, _} = :obr_lexer.string('()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('(1)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('(1,2)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # designator
    # {:ok, tokens, _} = :obr_lexer.string('tree')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree.left')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree.left.right')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree.left.right.up.down')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree[1][2][3]')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree(name).subnode')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree(name).subnode')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree(name).subnode(name)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('w[3].name[i]')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree[1][2]^[3]')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # identlist
    # {:ok, tokens, _} = :obr_lexer.string('day')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('day, month, year')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # fieldlist
    # {:ok, tokens, _} = :obr_lexer.string('day, month, year : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # fieldlistsequence
    # {:ok, tokens, _} = :obr_lexer.string('name, firstname: INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('name, firstname: INTEGER; a, b: WORD')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('name, firstname: INTEGER;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('name, firstname: INTEGER; a, b: WORD;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"


    # # type
    # {:ok, tokens, _} = :obr_lexer.string('blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY N0, N1, N2 OF blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('RECORD day, month, year: INTEGER \nEND')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY 10 OF blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY num OF blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # formaltype
    # {:ok, tokens, _} = :obr_lexer.string('XXX')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY OF XXX')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY OF ARRAY OF XXX')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # fpsection
    # # [VAR] ident {"," ident} ":" FormalType.
    # {:ok, tokens, _} = :obr_lexer.string('sukabl : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('sukabl, yat : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('VAR sukabl : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('VAR sukabl, yat : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # formalparameters
    # # FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
    # {:ok, tokens, _} = :obr_lexer.string('()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('() : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('(VAR x: INTEGER)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('(VAR x: INTEGER) : REAL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('(VAR x, y, z: INTEGER)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('(VAR x, y, z: INTEGER; VAR a,b:REAL; n: CHAR)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # type продолжение
    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE (a:integer)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE (VAR a:integer)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # type - RecordType
    # {:ok, tokens, _} = :obr_lexer.string('RECORD END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer; b:real END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer; b:real; END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('RECORD (Node) END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('RECORD (Node) a:integer; b:real; END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"


    # # type - pointer to
    # {:ok, tokens, _} = :obr_lexer.string('POINTER TO aaa')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # VariableDeclaration
    # {:ok, tokens, _} = :obr_lexer.string('a, bb, ccc, ddddd:INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # procedureheading
    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1(a:INTEGER)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1(a:INTEGER) : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1() : INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"


    # # length
    # {:ok, tokens, _} = :obr_lexer.string('1+2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # typedeclaration
    # {:ok, tokens, _} = :obr_lexer.string('Table       =  ARRAY N OF REAL')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string(' Tree        =  POINTER TO Node')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('Function*   =  PROCEDURE (x: INTEGER): INTEGER')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('  CenterNode  =  RECORD
    #                name: ARRAY 32 OF CHAR;
    #                subnode: Tree
    #              END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # constantdeclaration
    # {:ok, tokens, _} = :obr_lexer.string('N      =  100')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('limit  =  2*N -1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('all    =  {0 .. WordSize-1}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # label
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('"abc"')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('x')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # labelrange
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1..2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # caselabellist
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1, 2, 3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1..2, 5..6')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # assignment
    # {:ok, tokens, _} = :obr_lexer.string('x := 123')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # procedurecall
    # {:ok, tokens, _} = :obr_lexer.string('procedurename')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('procedurename()')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(1+1)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(2)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(xx)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(xx.yy.zz)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('procedurename(1,2,3)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # term
    # {:ok, tokens, _} = :obr_lexer.string('1*2/3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # statement
    # {:ok, tokens, _} = :obr_lexer.string('a := 1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # statementsequence
    # {:ok, tokens, _} = :obr_lexer.string('a := 1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"
    
    # {:ok, tokens, _} = :obr_lexer.string('a := 1; b:=2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # case
    # # TODO !!!!!!!!!! какие то проблемы с парсингом ноля
    # {:ok, tokens, _} = :obr_lexer.string('0 : x := x + y')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('55 : x := x + y')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('55..66 : x := x + y')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # ntcaselist
    # {:ok, tokens, _} = :obr_lexer.string('22 : x := x + y | 33 : x := x - y ')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # casestatement
    # {:ok, tokens, _} = :obr_lexer.string('CASE x OF 22 : x := x + y | 33 : x := x - y END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # IfStatement
    # {:ok, tokens, _} = :obr_lexer.string('IF x>1 THEN a:=0 END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('IF x>1 THEN a:=0 ELSIF x<1 THEN a:=-1 END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('IF x>1 THEN a:=0 ELSIF x<1 THEN a:=-1 ELSIF x=1 THEN a:=1 END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('IF x>1 THEN a:=0 ELSIF x<1 THEN a:=-1 ELSIF x=1 THEN a:=1 ELSE a:= 555 END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # repeatstatement
    # {:ok, tokens, _} = :obr_lexer.string('REPEAT a:=a+1 UNTIL a<100')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # whilestatement
    # str = '''
    # WHILE j > 0 DO
    #     j := j DIV 2; i := i+1
    # END
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # str = '''
    # WHILE m > n DO m := m - n
    # ELSIF n > m DO n := n - m
    # END
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # str = '''
    # WHILE m > n DO m := m - n
    # ELSIF n > m DO n := n - m
    # ELSIF n = m DO n := n * m
    # END
    # '''
    # {:ok, tokens, _} = :obr_lexer.string(str)
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # forstatement
    str = 'FOR v := beg TO end DO S END'
    {:ok, tokens, _} = :obr_lexer.string(str)
    Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    res = :obr_parser.parse(tokens)
    Logger.debug ">>>>>>>> res = #{inspect res}"

    str = 'FOR v := beg TO end BY inc DO S END'
    {:ok, tokens, _} = :obr_lexer.string(str)
    Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    res = :obr_parser.parse(tokens)
    Logger.debug ">>>>>>>> res = #{inspect res}"


  end
end
