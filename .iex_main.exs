# This is a configuration file for IEx, the Elixir REPL.
# We can load this main file into other project-specific .iex.exs
# with Code.eval_file("~/.iex_main.exs"), for example.
welcome_message = "(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ Loading personal configuration..."
|> String.pad_leading(50)

IO.puts(welcome_message)

IEx.configure(
    colors: [
      syntax_colors: [
        number: :magenta,
        atom: :cyan,
        string: :green,
        boolean: :magenta,
        nil: :read
      ],
      eval_result: [:green, :bright],
      eval_error: [[:red, :bright, "✘ \n"]],
      eval_info: [:yellow, :bright],
      eval_warning: [:yellow, :bright, "⚠"]],

  # Prompt configuration
    default_prompt:
      [
        # ANSI CHA, move cursor to column 1
        "\e[G",
        :light_magenta,
        # counter which helps us to reuse the previous results using v(counter)
        "%prefix",
        :blue,
        "(%counter)",
        :yellow,
        ">",
        :reset
      ]
      |> IO.ANSI.format()
      |> IO.chardata_to_string(),

      # IO.inspect configuration
      inspect: [
        limit: :infinity,
        charlists: :as_lists,
        pretty: true,
        binaries: :as_strings,
        printable_limit: :infinity
      ]
  )

# General imports and aliases
import ExUnit.Assertions
import_if_available(Ecto.Query)
