defmodule OberYaccTest do
  use ExUnit.Case

  require Logger

  # mix test --only yacc
  @tag yacc: true

  test "the truth" do   
    time1 = :os.system_time(:millisecond)
    {:ok, _} = :yecc.file('./priv/obr_parser.yrl')
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

    # # importlist
    # {:ok, tokens, _} = :obr_lexer.string('IMPORT system;')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('IMPORT system, crt;')
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

    # # identdef
    # {:ok, tokens, _} = :obr_lexer.string('ident1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ident1*')
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
    # {:ok, tokens, _} = :obr_lexer.string('1,2')
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

    # {:ok, tokens, _} = :obr_lexer.string('(1,2)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # set
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

    # # designator
    # {:ok, tokens, _} = :obr_lexer.string('tree.left.right.up.down')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('tree')
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

    # designator
    # {:ok, tokens, _} = :obr_lexer.string('alpha, beta, gamma')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # term
    # {:ok, tokens, _} = :obr_lexer.string('1+(2+3)+4')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # type
    # {:ok, tokens, _} = :obr_lexer.string('blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY 10+2 OF blah')
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

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE (a:integer)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # RecordType
    # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer; b:real; END') # !!!!!!!!!!!!!1
    # {:ok, tokens, _} = :obr_lexer.string('RECORD a:integer; b:real END')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # pointer to
    # {:ok, tokens, _} = :obr_lexer.string('POINTER TO aaa')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # VariableDeclaration
    # {:ok, tokens, _} = :obr_lexer.string('a, bb, ccc, ddddd:integer')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('ARRAY num OF blah')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # formalparameters
    # {:ok, tokens, _} = :obr_lexer.string('(a:integer)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # procedureheading
    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE *test1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE test1(a:integer)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('PROCEDURE *test1(a:integer)')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # forwarddeclaration
    {:ok, tokens, _} = :obr_lexer.string('PROCEDURE ^test1')
    Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    res = :obr_parser.parse(tokens)
    Logger.debug ">>>>>>>> res = #{inspect res}"

    {:ok, tokens, _} = :obr_lexer.string('PROCEDURE ^test1*')
    Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    res = :obr_parser.parse(tokens)
    Logger.debug ">>>>>>>> res = #{inspect res}"

    {:ok, tokens, _} = :obr_lexer.string('PROCEDURE ^test1*(a:integer)')
    Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    res = :obr_parser.parse(tokens)
    Logger.debug ">>>>>>>> res = #{inspect res}"

    {:ok, tokens, _} = :obr_lexer.string('PROCEDURE ^test1(a:integer)')
    Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    res = :obr_parser.parse(tokens)
    Logger.debug ">>>>>>>> res = #{inspect res}"

  end
end
