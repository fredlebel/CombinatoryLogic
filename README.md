Tools for playing with Combinatory Logic terms.
===============================================

Features:
- Compact SKI syntax with reduced parentheses.
- Custom symbols so new combinators can be defined.
- Supports abstractions to define a combinator in point form.
- Converts abstractions to SKI terms.
- Long identifiers for multi-character symbols.
- Predefined well known combinators.
- Syntax highlighting.
- Various options to control how the output is formatted.
- Output updates realtime as a combinator term is typed.

Examples
--------

### Symbols

    B=S(KS)K
    Q=(S(K(S(S(KD)(S(K(SB))(SI(K0))))))(S(S(KS)(S(S(KS)K)(K((SI(K0))))))(K((SI(K1))))))
    
### Abstractions

    [x.x(KI)xK]
    [a.[b.[c.c(ab)]]]

### Long identifiers

    {++}=[n.[f.[x.f(nfx)]]]
    {add}=[{n1}.[{n2}.{n1}{++}{n2}]]

### Reduced symbols

Use the normal form (NF) of a symbol.  Useful for reverse symbol matching when outputting.

For example, the symbol `{24}={mul}64` won't match the NF of `{factorial}4` which is a church number but the NF of `{24}` will.
So to do symbol matching on the NF of a symbol use the exclamation mark.
    
    {24}!{mul}64

Note that this will hang the program if defining a symbol which doesn't have a NF.

### Predefined combinators

As defined in _Lambda-Calculus and Combinators, an Introduction by J. ROGER HINDLEY and JONATHAN P. SELDIN_.

    B, C, W, U, Y, T, P, D, 0..9, Q, R

Screenshot
----------

![Screenshot](https://raw.github.com/fredlebel/CombinatoryLogic/master/screenshot.png)

Command line tool arguments
---------------------------

    Combinatory Logic Reducer
    -v             --version                 show version number
    -s SYMBOL      --symbol=SYMBOL           define a symbol and a combinator, ex: "B:S(KS)K"
    -S             --use_predefined_symbols  use the predefined symbols
    -L             --SKI                     shows only SKI combinators, no symbols
    -P             --show_parentheses        show full parentheses, ex: "((SK)I)"
    -f             --no_stop                 reduce until reaching NF, no stop at 200 iterations
    -l             --last                    only show the last line of the reduction
    -e             --eager                   tree reduction is done eagerly
    -c COMBINATOR  --combinator=COMBINATOR   combinator to reduce

