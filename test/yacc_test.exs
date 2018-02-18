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

    # tokens = [{:"[", 1}, {:atom, 1, :foo}, {:",", 1}, {:atom, 1, :bar}, {:"]", 1}]
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:t_lpar, 1, '('}, {:integer, 1, '123'}, {:t_rpar, 1, ')'},
              
    # {:ok, tokens, _} = :obr_lexer.string('123')
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('"some string"')
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # IdentDef
    # {:ok, tokens, _} = :obr_lexer.string('asd')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('asd-')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('asd*')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # IdentList
    # {:ok, tokens, _} = :obr_lexer.string('asd')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('asd*, blabla')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('asd*, blabla, cdef-')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # ExprList
    # {:ok, tokens, _} = :obr_lexer.string('1,2,3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1*1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1*1, 2*3')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # element
    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1..2')
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

    # {:ok, tokens, _} = :obr_lexer.string('{1,2}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('{1,5,7..9}')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # constexpr
    # {:ok, tokens, _} = :obr_lexer.string('1<2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # # constexpr
    # {:ok, tokens, _} = :obr_lexer.string('1..2')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # {:ok, tokens, _} = :obr_lexer.string('1')
    # Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    # res = :obr_parser.parse(tokens)
    # Logger.debug ">>>>>>>> res = #{inspect res}"

    # number
    {:ok, tokens, _} = :obr_lexer.string('123')
    Logger.debug ">>>>>>>> tokens = #{inspect tokens}"
    res = :obr_parser.parse(tokens)
    Logger.debug ">>>>>>>> res = #{inspect res}"

  end
end
