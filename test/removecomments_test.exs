defmodule RemoveCommentsTest do
  use ExUnit.Case

  require Logger

  # mix test --only removecomments
  @tag removecomments: true

  # test "removecomments file" do
  #   in_file_name = "./priv/mods/examples/BrightLetters.Mod"
  #   out_file_name = "./priv/mods/examples/BrightLetters_clean.Mod"

  #   OberLexx.Utils.file_remove_comments(in_file_name, out_file_name)
  # end

  test "removecomments binary" do
    charlist = ~c"""
        MODULE ASCII;(* some comment *)
        IMPORT system;

        END ASCII.
    """
    instr = to_string(charlist)

    # Logger.debug "#{inspect instr}"

    charlist_clean = ~c"""
        MODULE ASCII;
        IMPORT system;

        END ASCII.
    """
    truestr = to_string(charlist_clean)

    new_str = OberLexx.Utils.binary_remove_comments(instr)

    # Logger.debug "#{inspect new_str}"

    assert new_str === truestr
  end

end
