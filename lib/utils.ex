defmodule OberLexx.Utils do

  def remove_comments(in_file, out_file) do
    {:ok, binary} = File.read(in_file)
    file = File.open!(out_file, [:write])
    remove_comments_filter(binary, 0, file)
  end
  def remove_comments_filter(<<>>, _stack, file) do
    File.close(file)
  end
  def remove_comments_filter(<< "(*", tail :: binary >>, stack, file) do
    remove_comments_filter(tail, stack + 1, file)
  end
  def remove_comments_filter(<< "*)", tail :: binary >>, stack, file) do
    remove_comments_filter(tail, stack - 1, file)
  end
  def remove_comments_filter(<< "\n", tail :: binary >>, stack, file) do
    IO.binwrite(file, << "\n" >>)
    remove_comments_filter(tail, stack, file)
  end
  def remove_comments_filter(<< "\r", tail :: binary >>, stack, file) do
    IO.binwrite(file, << "\r" >>)
    remove_comments_filter(tail, stack, file)
  end
  def remove_comments_filter(<< header :: size(8), tail :: binary >>, 0, file) do
    IO.binwrite(file, << header :: size(8) >>)
    remove_comments_filter(tail, 0, file)
  end
  def remove_comments_filter(<< _header :: size(8), tail :: binary >>, stack, file) do
    remove_comments_filter(tail, stack, file)
  end

end
