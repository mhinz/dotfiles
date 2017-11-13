IEx.configure [
  colors: [
    eval_info:       [:yellow, :bright],
    eval_error:      [:red],
    eval_interrupt:  [:red],
    stack_info:      [:blue],
    doc_code:        [:blue],
    doc_inline_code: [:blue],
    doc_headings:    [:red, :bright],
  ],
  width: 100,
  default_prompt: [
    "\e[G",
    :blue,
    :bright,
    "%prefix(%counter)>",
  ] |> IO.ANSI.format |> IO.chardata_to_string
]

defmodule Mex do
  defp expand_all(n, env) do
    Macro.prewalk(n, &Macro.expand(&1, env))
  end

  defmacro mex(do: block) do
    block
    |> expand_all(__CALLER__)
    |> Macro.to_string
    |> IO.puts

    quote do: :ok
  end
end
