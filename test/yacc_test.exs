defmodule OberYaccTest do
  use ExUnit.Case

  require Logger

  # mix test --only yacc
  @tag yacc: true  

  test "the truth" do
    
    # {:ok, _} = :leex.file('./priv/obr.xrl')

    # {:ok, :obr} = :c.c('./priv/obr.erl')



  end
end
