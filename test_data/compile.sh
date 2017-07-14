elixirc -o . test.ex
elixir -e "File.write!(\"decomp.ex\", inspect(:beam_disasm.file('Elixir.Test'), pretty: true))"
