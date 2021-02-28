### QCC

The first 131 lines are a simple tokenizer. Functionality is mimicked in the first 112 lines of [`mcc.py`](mcc.py). Noteworthy points about the tokenizer are:
- String literals are written to `glo` (`strings` in `mcc.py`).
- Identifiers are assigned integers.

Lines 133 through 259 define multiple utilities that will be used for code generation. The `patch` function recursively patches destinations of relative or absolute jumps. Note that a linked list structure is created in `obuf` is more than one jump needs to patched to the same destination. The `patchlval` function is used to either delete the last instructions or turn a `mov` instruction into a `lea` instruction. `patchlval` should only be invoked right after `lval` is set.

The next 140-ish lines (261 to 395) define the expression parser/code generator.

### Resources

[QCC - A Quick C Compiler](https://c9x.me/qcc/)

[Shunting yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
