# Pengine demo site

* [ ] Prolog Shell
  - [X] Resize: allow changing widths of sub-windows.
  - [X] Separate answer blocks (query-enter/answer inside output window?)
  - [ ] New examples show up only after reload
  - [X] Complete rebound I/O (also in examples).
  - [ ] Clearer feedback that we are waiting for input
  - [X] Make messages work.
  - [X] Make time/1 work.

  - [ ] Redesign:
    - [ ] Modularize into
      - [ ] Editor
      - [ ] Interactor (output, query selection)
        - [ ] Have a one-line query select/input and [run] with
          hidden/popup (preferences) output window.
    - [ ] Allow adding SWISH to a tutorial similar to this:

```prolog
p(X) :- q(X).
p(a).
```

* [ ] Scratchpad:
  - [ ] Make clearer that `Scratchpad` is a link back home.
  - [X] Have pengine icon in the top-left
  - [ ] Show current example
  - [ ] Make it clearer that you can make your own changes and version
  - [ ] Examples
    - [ ] Client Side Templating: Does not work with `ask-in-create'.
  - [ ] Wordnet application and demos.

* [ ] Admin
  - [X] Remove irrelevant settings (e.g., listing).

# Editor

  - [ ] Ace or CodeMirror
    - [ ] Can be replaced; both seem to work with TogetherJS.

# TogetherJS

  - [ ] Further hooks to improve syncing?

# Tutorial pages with embedded SWISH
