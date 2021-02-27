### QCC

The first 131 lines are a simple tokenizer. Functionality is mimicked in the first 112 lines of [`mcc.py`](mcc.py). Noteworthy points about the tokenizer are:
- String literals are written to `glo` (`strings` in `mcc.py`).
- Identifiers are assigned integers.

### Resources

[QCC - A Quick C Compiler](https://c9x.me/qcc/)

[Shunting yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
