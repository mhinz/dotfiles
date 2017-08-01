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
