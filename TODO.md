# todo

- bias the agent (through the prompts) to strongly prefer the ocaml tools instead of the generic ones
- text selection
- consider always using a local model (e.g. gpt oss) as a small model for titles, for what the subagents are doing to display in the subagent view tui, for recap in the transcript, etc.
- consider owning a dune rpc that builds in a separate build dir so we don't conflict with tools like describe projects that are incompatible with dune rpc running. Gives us both live diagnostics, and doesn't block running incompatible dune commands
- improve the eval and drive inspection of sessions to fix issues
  - consider task specific evals like docs and design that have a reference to test against
- consider augmenting describe to give module tree per library. and other useful info?
- manual (from doc/manual) rendered in www/
