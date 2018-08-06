defmodule OberLexx.Utils do
  require Logger

  def file_remove_comments(in_file, out_file) do
    {:ok, binary} = File.read(in_file)
    # t1 = raw_binary_to_string(binary)
    # IO.puts(t1)
    new_binary = binary_remove_comments(binary)
    # t2 = raw_binary_to_string(new_binary)
    # IO.puts(t2)

    file = File.open!(out_file, [:write])
    IO.binwrite(file, new_binary)
    File.close(file)
  end

  def binary_remove_comments(in_binary) do
    binary_remove_comments(in_binary, 0, <<>>)
  end
  def binary_remove_comments(<<>>, _stack, out_binary) do
    out_binary
  end
  def binary_remove_comments(<< "(*", tail :: binary >>, stack, out_binary) do
    binary_remove_comments(tail, stack + 1, out_binary)
  end
  def binary_remove_comments(<< "*)", tail :: binary >>, stack, out_binary) do
    binary_remove_comments(tail, stack - 1, out_binary)
  end
  def binary_remove_comments(<< "\n", tail :: binary >>, stack, out_binary) do
    binary_remove_comments(tail, stack, out_binary <> << "\n" >>)
  end
  def binary_remove_comments(<< "\r", tail :: binary >>, stack, out_binary) do
    binary_remove_comments(tail, stack, out_binary <> << "\r" >>)
  end
  def binary_remove_comments(<< header :: size(8), tail :: binary >>, 0, out_binary) do
    binary_remove_comments(tail, 0, out_binary <> << header :: size(8) >>)
  end
  def binary_remove_comments(<< _header :: size(8), tail :: binary >>, stack, out_binary) do
    binary_remove_comments(tail, stack, out_binary)
  end

  def raw_binary_to_string(raw) do
    codepoints = String.codepoints(raw)  
    Enum.reduce(codepoints, fn(w, result) ->  
                                cond do 
                                  String.valid?(w) -> 
                                      result <> w 
                                  true ->
                                      << parsed :: 8>> = w 
                                      result <>   << parsed :: utf8 >>
                                end
                            end)
  end

end
