# issues

1. compilation warnings
In file included from /root/mentat/_build/_private/default/.pkg/ocaml-compiler.5.5.0-593bf60bbf5608a281f10010ec51bce8/target/lib/ocaml/caml/callback.h:22,
                 from mentat_fswatch_fsevents_stubs.c:2:
mentat_fswatch_fsevents_stubs.c: In function ‘mentat_file_watcher_fsevents_create’:
/root/mentat/_build/_private/default/.pkg/ocaml-compiler.5.5.0-593bf60bbf5608a281f10010ec51bce8/target/lib/ocaml/caml/memory.h:302:29: warning: unused variable ‘caml__frame’ [-Wunused-variable]
  302 |   struct caml__roots_block *caml__frame = *caml_local_roots_ptr
      |                             ^~~~~~~~~~~
/root/mentat/_build/_private/default/.pkg/ocaml-compiler.5.5.0-593bf60bbf5608a281f10010ec51bce8/target/lib/ocaml/caml/memory.h:313:3: note: in expansion of macro ‘CAMLparam0’
  313 |   CAMLparam0 (); \
      |   ^~~~~~~~~~
mentat_fswatch_fsevents_stubs.c:311:3: note: in expansion of macro ‘CAMLparam3’
  311 |   CAMLparam3(v_path, v_latency, v_callback);
      |   ^~~~~~~~~~
mentat_fswatch_fsevents_stubs.c: In function ‘mentat_file_watcher_fsevents_stop’:
/root/mentat/_build/_private/default/.pkg/ocaml-compiler.5.5.0-593bf60bbf5608a281f10010ec51bce8/target/lib/ocaml/caml/memory.h:302:29: warning: unused variable ‘caml__frame’ [-Wunused-variable]
  302 |   struct caml__roots_block *caml__frame = *caml_local_roots_ptr
      |                             ^~~~~~~~~~~
/root/mentat/_build/_private/default/.pkg/ocaml-compiler.5.5.0-593bf60bbf5608a281f10010ec51bce8/target/lib/ocaml/caml/memory.h:305:3: note: in expansion of macro ‘CAMLparam0’
  305 |   CAMLparam0 (); \
      |   ^~~~~~~~~~
mentat_fswatch_fsevents_stubs.c:316:3: note: in expansion of macro ‘CAMLparam1’
  316 |   CAMLparam1(v_t);
      |   ^~~~~~~~~~

2. doesn't seem to persist the effort of the model on exit, it always comes back to medium effort even after I select xhigh

3. Can't use '?' from home screen
4. some commands don't make sense to have in the home screen, for instance:
            ❯ /clear      Start a new session with empty context; previous session stays on disk
              /fork       Fork current session
              /rewind     Jump back to an earlier message and resubmit
              /undo       Undo the last turn: revert its files and reload its message
              /redo       Redo an undone turn, restoring its files and messages
5. The "[]" or "<>" tag on the command is to far on the right, it should be closer to the description, otherwise it's not visible at all
/goal       Declare a goal, or inspect and control the current one                                            [objective]
  /init       Generate or update AGENTS.md with project conventions
❯ /rename     Rename the active session                                                                         <title>
  /review     Review the worktree diff against the base
  /model      Select model and effort
6. I don't know how I did it but I managed to loose the focus of the composer. Ctrl+C still works, but there's nothing I can do to focus back the composer to type things. Tab doesn't work, click doesn't work.