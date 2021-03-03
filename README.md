### QCC

The first 131 lines are a simple tokenizer. Functionality is mimicked in the first 112 lines of [`mcc.py`](mcc.py). Noteworthy points about the tokenizer are:
- String literals are written to `glo` (`strings` in `mcc.py`).
- Identifiers are assigned integers.

Lines 133 through 259 define multiple utilities that will be used for code generation. The `patch` function recursively patches destinations of relative or absolute jumps. Note that a linked list structure is created in `obuf` is more than one jump needs to patched to the same destination. The `patchlval` function is used to either delete the last instructions or turn a `mov` instruction into a `lea` instruction. `patchlval` should only be invoked right after `lval` is set.

The next 140-ish lines (261 to 395) define the expression parser/code generator.

### C Grammar

The following is a simplified version of the grammar available in Annex A of [the C11 standard](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf).

**Expression**

    primary-expression:
        identifier
        constant
        string-literal
        '(' expression ')'

    postfix-expression:
        primary-expression
        postfix-expression '(' ')'
        postfix-expression '(' expression-list ')'

    expression-list:
        expression
        expression-list ',' expression

    unary-expression:
        postfix-expression
        '&' unary-expression
        '*' unary-expression
        '+' unary-expression
        '-' unary-expression
        '~' unary-expression
        '!' unary-expression

    multiplicative-expression:
        unary-expression
        multiplicative-expression '*' unary-expression
        multiplicative-expression '/' unary-expression
        multiplicative-expression '%' unary-expression

    additive-expression:
        multiplicative-expression
        additive-expression '+' multiplicative-expression
        additive-expression '-' multiplicative-expression

    shift-expression:
        additive-expression
        shift-expression '<<' additive-expression
        shift-expression '>>' additive-expression

    relational-expression:
        shift-expression
        relational-expression '<' shift-expression
        relational-expression '>' shift-expression
        relational-expression '<=' shift-expression
        relational-expression '>=' shift-expression

    equality-expression:
        relational-expression
        equality-expression '==' relational-expression
        equality-expression '!=' relational-expression

    and-expression:
        equality-expression
        and-expression '&' equality-expression

    exclusive-or-expression:
        and-expression
        exclusive-or-expression '^' and-expression

    inclusive-or-expression:
        exclusive-or-expression
        inclusive-or-expression '|' exclusive-or-expression

    expression:
        inclusive-or-expression
        unary-expression '=' expression

**Declaration**

    itendifier-list:
        itendifier
        itendifier-list ',' itendifier

    declaration:
        'int' itendifier-list ';'

**Statement**

    statement:
        compound-statement
        expression-statement
        'if' '(' expression ')' statement
        'if' '(' expression ')' statement 'else' statement
        'while' '(' expression ')' statement

    compound-statement:
        '{' '}'
        '{' statement-list '}'
        '{' declaration-list '}'
        '{' declaration-list statement-list '}'

    declaration-list:
        declaration
        declaration-list declaration

    statement-list:
        statement
        statement-list statement

    expression-statement:
        ';'
        expression ';'

### Resources

https://c9x.me/qcc/

https://en.wikipedia.org/wiki/Shunting-yard_algorithm

http://www.reedbeta.com/blog/the-shunting-yard-algorithm/#advanced-usage

http://wcipeg.com/wiki/Shunting_yard_algorithm#Unary_operators

http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf

https://www.lysator.liu.se/c/ANSI-C-grammar-y.html
