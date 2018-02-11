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

    tokens = [{:"[", 1}, {:atom, 1, :foo}, {:",", 1}, {:atom, 1, :bar}, {:"]", 1}]
    res = :obr_parser.parse(tokens)
    Logger.debug ">>>>>>>> res = #{inspect res}"

  end
end
