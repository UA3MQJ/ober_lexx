defmodule RemoveCommentsTest do
  use ExUnit.Case

  require Logger

  # mix test --only removecomments
  @tag removecomments: true  

  test "removecomments" do
    in_file_name = "./priv/mods/examples/Laser2Demo.Mod"
    out_file_name = "./priv/mods/examples/Laser2Demo_clean.Mod"

    OberLexx.Utils.remove_comments(in_file_name, out_file_name)
  end

end
