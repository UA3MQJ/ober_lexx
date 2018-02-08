defmodule OberLexx do
  use Application
  require Logger

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def tokenize(oberon_str) do
    state = %{cb_counter: 0,
              comment: false,
              comment_str: "",
              token_line: 1,
              errors: []}
    tokenize(oberon_str, [], state)
  end

  defp tokenize(["(*"<>oberon_str, tokens, state) do
    new_state = %{state | token_line: state.token_line,
                          comment: true,
                          comment_str: state.comment_str <> "(*",
                          cb_counter: state.cb_counter + 1 }
    tokenize(oberon_str, tokens, new_state)
  end


  # # токенизируем вложенные камменты
  # defp tokenize([?(,?*|oberon_str], tokens, state) do
  #   new_state = %{state | token_line: state.token_line,
  #                         comment: true,
  #                         comment_str: state.comment_str <> "(*",
  #                         cb_counter: state.cb_counter + 1 }
  #   tokenize(oberon_str, tokens, new_state)
  # end
  # defp tokenize([?*,?)|oberon_str], tokens, %{comment: false} = state) do
  #   tokenize([], [{:illegal, "*)", state.token_line}|tokens], state)
  # end
  # defp tokenize([?*,?)|oberon_str], tokens, %{comment: true} = state) do
  #   new_cb_counter = state.cb_counter - 1
  #   cond do
  #     # комментарий закончился
  #     new_cb_counter == 0 ->
  #       new_state = %{state | token_line: state.token_line,
  #                             comment: false,
  #                             comment_str: "",
  #                             cb_counter: new_cb_counter }
  #       tokenize(oberon_str, [{:comments, state.comment_str <> "*)", state.token_line}|tokens], new_state)
  #     # вложенность комментария уменьшается, но он еще не кончился
  #     new_cb_counter > 0 ->
  #       new_state = %{state | token_line: state.token_line,
  #                             cb_counter: new_cb_counter,
  #                             comment_str: state.comment_str <> "*)",
  #                             cb_counter: new_cb_counter }
  #       tokenize(oberon_str, tokens, new_state)
  #   end
  # end
  # defp tokenize([?\n|oberon_str], tokens, %{comment: true} = state) do
  #   new_state = %{state | comment_str: state.comment_str <> "\n",
  #                         token_line: state.token_line + 1}
  #   tokenize(oberon_str, tokens, new_state)
  # end
  # defp tokenize([sym|oberon_str], tokens, %{comment: true} = state) do
  #   new_state = %{state | comment_str: state.comment_str <> to_string([sym]) }
  #   tokenize(oberon_str, tokens, new_state)
  # end


  # defp tokenize([], tokens, state) do
  #   Logger.debug ">>>>> state=#{inspect state}"
  #   new_tokens = cond do
  #     state.cb_counter > 0  ->
  #       [{:illegal, "*)", state.token_line}|tokens]
  #     true ->
  #       tokens
  #   end
  #   Enum.reverse(new_tokens)
  # end

end
