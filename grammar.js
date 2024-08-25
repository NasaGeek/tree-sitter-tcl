// General "issues" that persist:
// hardcoded {} in various spots which should actually be interchangeable with ""
//  except it's a thorny issue, so probably will just force braces for most places
// poor bareword (simple_word) special character handling
//  not sure this is worth fixing; just quote your stuff!
// unbraced exprs and switches unsupported
// exprs generally need whitespace between operators/operands

const PREC = {
  unary        : 150,
  exp          : 140,
  muldiv       : 130,
  addsub       : 120,
  shift        : 110,
  compare      : 100,
  equal_bool   : 90,
  equal_string : 80,
  contain      : 70,
  and_bit      : 60,
  xor_bit      : 50,
  or_bit       : 40,
  and_logical  : 30,
  or_logical   : 20,
  ternary      : 10,
  comma        :  5,
}

const interleaved1 = (rule, delim) => seq(rule, repeat(seq(delim, rule)))
const interleavednl1 = (rule, delim) => seqnl(rule, repeat(seqnl(delim, rule)))

// Helper for rules within which newlines are irrelevant (essentially adding
// them as faux-extras).
const seqnl = (...rules) => seq(...rules.flatMap(e => [repeat("\n"), e]).slice(1))

// A lot of stealing from tree-sitter-go and then simplifying for Tcl
const hexDigit = /[0-9a-fA-F]/;
const octalDigit = /[0-7]/;
const decimalDigit = /[0-9]/;
const binaryDigit = /[01]/;

const hexDigits = repeat1(hexDigit);
const octalDigits = repeat1(octalDigit);
const decimalDigits = repeat1(decimalDigit);
const binaryDigits = repeat1(binaryDigit);

const hexLiteral = seq('0', choice('x', 'X'), hexDigits);
const octalLiteral = seq('0', optional(choice('o', 'O')), octalDigits);
const decimalLiteral = choice('0', seq(/[1-9]/, optional(decimalDigits)));
const binaryLiteral = seq('0', choice('b', 'B'), binaryDigits);

const intLiteral = choice(binaryLiteral, decimalLiteral, octalLiteral, hexLiteral);

const decimalExponent = seq(choice('e', 'E'), optional(choice('+', '-')), decimalDigits);
const decimalFloatLiteral = choice(
  seq(decimalDigits, '.', optional(decimalDigits), optional(decimalExponent)),
  seq(decimalDigits, decimalExponent),
  seq('.', decimalDigits, optional(decimalExponent)),
);

const infLiteral = /inf/i;
const nanLiteral = /nan/i;

const floatLiteral = choice(infLiteral, nanLiteral, decimalFloatLiteral);
// theft over

module.exports = grammar({
  name: 'tcl',

  word: $ => $.simple_word,

  externals: $ => [
    // looks for if next token is alpha, $, [, or _
    // Covers separation of tokens that aren't whitespace-separated, think of
    // cases like [func][func] or a$a. Not sure what purpose the _ serves, though
    $.concat,
    // looks for :: followed by alpha
    $._ns_delim
  ],

  inline: $ => [
    $._commands,
    $._builtin,
    $._terminator,
    $._word,
  ],

  extras: $ => [
    $._line_continuation,
    // https://www.tcl-lang.org/cgi-bin/tct/tip/407.html#:~:text=Tcl%27s%20source%20code.-,String%20Representation%20of%20Lists,-The%20routines%20%5Bin
    // Don't blanket-ignore whitespace, because newlines are significant
    /[ \t\v\f\r]/,
  ],

  conflicts: $ => [
    // The func ones cause the absolute strangest errors when removed. It's
    // somehow related to allowing arbitrary newlines between things.
    [$.func_call],
    [$.func_args],

    [$.foreach_clauses],
    [$.binop_expr, $.ternary_expr],
    [$.switch_body],
  ],

  rules: {
    // https://wiki.tcl-lang.org/page/the+empty+command
    source_file: $ => choice($._commands, repeat($._terminator)),

    // https://www.tcl-lang.org/man/tcl8.6/TclCmd/Tcl.htm#M5
    _commands: $ => seq(
      repeat($._terminator),
      interleaved1($._command, repeat1($._terminator)),
      repeat($._terminator)
    ),

    _line_continuation: _ => token(seq('\\', choice(seq(optional('\r'), '\n'), '\0'))),

    _terminator: _ => choice('\n', ';'),

    comment: _ => /#(\\(.|\r?\n)|[^\\\n])*/,

    _builtin: $ => choice(
      $.conditional,
      $.global,
      $.namespace,
      $.procedure,
      $.set,
      $.try,
      $.for,
      $.foreach,
      $.regexp,
      $.regsub,
      $.switch,
      $.expr_cmd,
      $.while,
      $.catch,
    ),

    while: $ => seq('while', $.expr, $.script),

    expr_cmd: $ => seq('expr', $.expr),

    regexp: $ => seq('regexp', repeat1($._word)),
    regsub: $ => seq('regsub', repeat1($._word)),

    for: $ => seq("for",
      $.script,
      $.expr,
      $.script,
      $.script,
    ),

    // https://www.tcl.tk/man/tcl/TclCmd/foreach.htm
    foreach: $ => seq("foreach",
      $.foreach_clauses,
      $.script,
    ),

    // Unhiding either of these causes a bad parse in some cases
    foreach_clauses: $ => (repeat1($.foreach_clause)),
    foreach_clause: $ => seq($._word, $._word),


    // https://www.tcl.tk/man/tcl/TclCmd/switch.htm
    switch: $ => seq("switch",
      field("flags", repeat($._word)),
      field("pattern", $._word),
      $.switch_body,
    ),

    switch_body: $ => choice(
      // Trouble with newlines, doesn't seem very useful anyway.
      // Also makes it hard to properly detect the switch body because a case
      // like `switch -flag pattern { ... }` looks like `-flag` is the pattern
      // and then `pattern` is the first case. Unless there's some precedence
      // magic that would work, I can only think of doing smarter parsing such
      // that the specific flags are recognized.
      // $._inner_switch,

      // We were previously a bit on the strict side (honoring the actual Tcl
      // behavior) by disallowing empty or whitespace-filled {}. Unfortunately
      // parsing of later sibling constructs then ended up broken by an empty
      // {}. Making it more permissive seems like the better call.
      seqnl(
        token(prec(1, "{")),
        optional(interleaved1($._inner_switch, repeat('\n'))),
        "}"
      ),
    ),

    _inner_switch: $ => seqnl(
      // This isn't really accurate since the patterns are interpreted "raw"
      // with no substitution. Very similar to proc arg defaults actually.
      $._word,
      // Ehh not totally sold on the aliasing
      choice($.script, alias('-', $.script)),
    ),

    // evaluated,
    // terminator-delimited commands,
    // surrounded by "", {}, or nothing (though in the nothing case it must not have whitespace)
    // Name is slighty awkward, since we don't _know_ things will be eval'ed as
    // scripts. We could call it maybe_script?
    script: $ => choice(
      $._concat_word,
      seq("{", choice($._commands, repeat($._terminator)), "}"),
      // Broadly applicable quotes are really tough, need to ban them in
      // certain constructs or otherwise figure out how to deal with them
      // (custom lexing?)
      // seqnl('"', choice($._commands, repeat($._terminator)), '"'),
    ),

    global: $ => seq("global", repeat($._word)),

    namespace: $ => seq('namespace', $._namespace_subcommand),

    _namespace_subcommand: $ => choice(
      seq("eval", $._word_eval_list),
      $._word_eval_list,
    ),

    try: $ => seq(
      "try",
      $.script,
      repeat(choice(
        seq(
          "on",
          choice(
            "ok", "error", "return", "break", "continue", /[0-4]/
          ),
          $._word,
          $.script,
        ),
        seq(
          "trap",
          $._word,
          $._word,
          $.script,
        ))
      ),
      optional($.finally),
    ),

    finally: $=> seq('finally', $.script),

    _command: $ => choice(
      $._builtin,
      $.comment,
      $.command
    ),

    // commands are just _word's, okay
    command: $ => seq(
      field('name', $._word),
      optional(field('arguments', $._word_eval_list)),
    ),

    _word_eval_list: $ => repeat1($._word_eval),

    // https://www.tcl-lang.org/man/tcl8.6/TclCmd/Tcl.htm#M9
    unpack: _ => '{*}',

    // A word that might possibly be evaluated as code
    _word_eval: $ => seq(
      optional($.unpack),
      $.script,
    ),

    // A word that we know for sure will never be evaluated as code (basically
    // only for use in builtins where the behavior is known)
    _word: $ => seq(
      optional($.unpack),
      choice($.braced_word, $._concat_word)
    ),

    // Might end up useful for arbitrary quoted stuff...
    // _concat_word_noquote: $ => interleaved1(
    //   choice(
    //     $.escaped_character,
    //     $.command_substitution,
    //     $.simple_word,
    //     $.varname,
    //     $.variable_substitution,
    //   ),
    //   $.concat,
    // ),

    // All the stuff that can be mashed together without needing whitespace
    // delimiters. These are generally the constructs that undergo first-pass
    // evaluation.
    _concat_word: $ => interleaved1(
      choice(
        $.escaped_character,
        $.command_substitution,
        $.quoted_word,
        $.variable_substitution,
        $.simple_word,
        $.varname,
      ),
      $.concat,
    ),

    // bare words are a no-no inside of expr's
    _concat_word_expr: $ => interleaved1(
      choice(
        $.escaped_character,
        $.command_substitution,
        $.quoted_word,
        $.variable_substitution,
      ),
      $.concat,
    ),

    // Specifically (only?) useful for variable substitution due to
    // token.immediate
    _ident: _ => token.immediate(/[a-zA-Z_][a-zA-Z0-9_]*/),

    id: $ => seq(optional($._ns_delim), interleaved1($._ident, $._ns_delim)),

    // token.immediate breaks usage of () in bare words (maybe that's okay?)
    // e.g. `puts (string)` should work fine, treat (string) as a string.
    // maybe it makes sense to include () in simple_word or parse arrays in
    // scanner.c?
    // We want token.immediate so abc (xyz) isn't treated as an array, though.
    // How do we let functions in exprs take precedence? Why is this stealing precedence?
    array_index: $ => seq(token.immediate('('), $._concat_word, ')'),

    // cheating here a bit I think by oversimplifying what an array reference can be
    array_name: $ => seq($.simple_word, $.array_index),

    _array_ref: $ => seq('$', $.id, $.array_index),

    varname: $ => choice(
      $.array_name,
    ),

    variable_substitution: $ => seq(
      choice(
        seq('$', $.id),
        $._array_ref,
        seq('$', '{', /[^}]+/, '}'),
      ),
    ),

    set: $ => seq("set",
      // TODO: Change this up for arrays
      $._word,
      optional($._word)),

    procedure: $ => seq(
      "proc",
      field('name', $._word),
      field('arguments', $.arguments),
      field('body', $.script)
    ),

    arguments: $ => choice(
      seq('{', repeat($.argument), '}'),
      $._concat_word,
    ),

    argument: $ => choice(
      field('name', $.simple_word),
      seq(
        '{',
         // More strict than Tcl, a convenience
         field('name', $.simple_word),
         optional(field('default', $._argument_word)),
         '}'
      )
    ),

    // quoted_word here isn't quite right because the argument is interpreted
    // literally. Really should just allow roughly anything (close to braced_word)
    _argument_word: $ => choice($.simple_word, $.quoted_word, $.braced_word),

    // FIXME: errors out on certain braced expressions without spaces between
    // operators/operands due to simple_words allowing operators within them.
    // Gotta do something about the function call support here or adjust
    // simple_word.
    //
    // expr in general is such a spin on Tcl's usual syntax that it's quite
    // difficult to support well alongside many of Tcl's other idiosyncracies.
    _expr: $ => choice(
      seqnl("(", $._expr, ")"),
      $.int_literal,
      $.float_literal,
      $.bool_literal,
      $.unary_expr,
      $.binop_expr,
      $.ternary_expr,
      $._concat_word_expr,
      $.func_call,
      $.braced_word,
    ),

    expr: $ => choice(
      // prec disambiguates from braced_word
      seqnl(token(prec(1, '{')), $._expr, '}'),
      // Might be easier to support some subset of expr's when unbraced (e.g.
      // _concat_word and some literals). I've got way too much newline going
      // on in _expr to be able to handle them fully. Besides, it's bad practice.
      // I probably need to at least support single-arg expr's though.
      // $._expr,
      // Not delighted with this since it produces simple_words in some cases
      // rather than more specific literals.
      $._concat_word,
    ),

    func_call: $ => seqnl(
        field("name", $.restricted_simple_word),
        "(",
        field("args", optional($.func_args)),
        ")"
      ),

    func_args: $ => interleavednl1($._expr, ","),

    unary_expr: $ => prec.left(PREC.unary, seqnl(choice("-", "+", "~", "!"), $._expr)),

    binop_expr: $ => choice(
      prec.left(PREC.exp,          seqnl($._expr, "**",  $._expr)),

      prec.left(PREC.muldiv,       seqnl($._expr, "/",  $._expr)),
      prec.left(PREC.muldiv,       seqnl($._expr, "*",  $._expr)),
      prec.left(PREC.muldiv,       seqnl($._expr, "%",  $._expr)),
      prec.left(PREC.addsub,       seqnl($._expr, "+",  $._expr)),
      prec.left(PREC.addsub,       seqnl($._expr, "-",  $._expr)),

      prec.left(PREC.shift,        seqnl($._expr, "<<", $._expr)),
      prec.left(PREC.shift,        seqnl($._expr, ">>", $._expr)),

      prec.left(PREC.compare,      seqnl($._expr, ">",  $._expr)),
      prec.left(PREC.compare,      seqnl($._expr, "<",  $._expr)),
      prec.left(PREC.compare,      seqnl($._expr, ">=", $._expr)),
      prec.left(PREC.compare,      seqnl($._expr, "<=", $._expr)),

      prec.left(PREC.equal_bool,   seqnl($._expr, "==", $._expr)),
      prec.left(PREC.equal_bool,   seqnl($._expr, "!=", $._expr)),

      prec.left(PREC.equal_string, seqnl($._expr, "eq", $._expr)),
      prec.left(PREC.equal_string, seqnl($._expr, "ne", $._expr)),

      prec.left(PREC.contain,      seqnl($._expr, "in", $._word)),
      prec.left(PREC.contain,      seqnl($._expr, "ni", $._word)),

      prec.left(PREC.and_bit,      seqnl($._expr, "&", $._expr)),
      prec.left(PREC.xor_bit,      seqnl($._expr, "^", $._expr)),
      prec.left(PREC.or_bit,       seqnl($._expr, "|", $._expr)),

      prec.left(PREC.and_logical,  seqnl($._expr, "&&", $._expr)),
      prec.left(PREC.or_logical,   seqnl($._expr, "||", $._expr)),
    ),

    ternary_expr: $ => prec.left(PREC.ternary, seqnl($._expr, '?', $._expr, ':', $._expr)),

    conditional: $ => seq(
      "if",
      field('condition', $.expr),
      optional("then"),
      $.script,
      repeat($.elseif),
      optional($.else),
    ),

    elseif: $ => seq(
      "elseif",
      field('condition', $.expr),
      optional("then"),
      $.script,
    ),

    else: $ => seq(
      "else",
      $.script,
    ),

    catch: $ => seq(
      "catch",
      $.script,
      optional(seq($._word, optional($._word)))
    ),

    quoted_word: $ => seq(
      '"',
      repeat(choice(
        $.variable_substitution,
        $._quoted_word_content,
        $.command_substitution,
        $.escaped_character,
      )),
      '"',
    ),

    _braced_word_contents: _ => /[^{}]+/,

    _nested_braces: $ => seq('{', repeat(choice($._nested_braces, $._braced_word_contents)), '}'),

    // This a truly braced, no-substitution word, only appropriate in places
    // where we know no evaluation will occur (anywhere that it _might_ occur
    // should be a _word).
    // It can have almost anything in it I think (other than unmatched {})
    // FIXME: escaped braces
    braced_word: $ => seq('{', repeat(choice($._nested_braces, $._braced_word_contents)), '}'),


    raw_word_contents: _ => token(/[^{}\s]+/),
    // Need external lexer for this I think, essentially trying to not match
    // on spaces unless they're between braces, but extras are getting in my way
    // raw_word: $ => repeat1(choice($.raw_word_contents, $.braced_word)),

    escaped_character: _ => /\\./,

    // https://github.com/tree-sitter/tree-sitter/issues/1087#issuecomment-833198651
    _quoted_word_content: _ => token(prec(-1, /[^$\\\[\]"]+/)),

    command_substitution: $ => seq('[', $._command, ']'),

    bool_literal: _ => token(prec.dynamic(-2, /(((t)r?)u?)e?|((((f)a?)l?)s?)e?|on|(of)f?|((y)e?)s?|(n)o?/i)),

    int_literal: _ => token(prec.dynamic(-2, intLiteral)),
    float_literal: _ => token(prec.dynamic(-2, floatLiteral)),

    // I'd kind of like to remove () from the exclusion for matching array names,
    // but there are knock-on effects like degraded recognition of function calls in exprs
    simple_word: _ => token(prec.dynamic(-1, /[^!$\s\\\[\]{}();"]+/)),

    // Helps us out in exprs (though we're violating Tcl's fun "name everything
    // whatever you want" behavior)
    restricted_simple_word: _ => token(/[A-Za-z_][A-Za-z0-9_]*/),
  }

  });
