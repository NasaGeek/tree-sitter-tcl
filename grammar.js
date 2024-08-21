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
}

const interleaved1 = (rule, delim) => seq(rule, repeat(seq(delim, rule)))

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
    // I don't really understand what this is all about but I think it's covering
    // cases like [func][func]?
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
    // concerning, as whitespace is quite significant in Tcl. We probably want
    // more control over it
    // /\s/,
    // https://www.tcl-lang.org/cgi-bin/tct/tip/407.html#:~:text=Tcl%27s%20source%20code.-,String%20Representation%20of%20Lists,-The%20routines%20%5Bin
    /[ \t\v\f\r]/,
    // also possibly suspicious as you can't just have newlines wherever
    // also doesn't the above rule handle this? Oh unless it's not multiline!
    // /\\\r?\n/
  ],

  conflicts: $ => [
    [$.foreach_clauses],
    [$._concat_word, $._concat_word_expr],
    [$.braced_word_simple, $._expr]
  ],

  rules: {
    source_file: $ => $._commands,

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
      $.foreach,
      $.expr_cmd,
      $.while,
      $.catch,
    ),

    while: $ => seq('while', $.expr, $._word),

    expr_cmd: $ => seq('expr', $.expr),

    // arguments doesn't make sense here. That's for procs
    // foreach var {list of stuff} {code}
    // foreach {var1 var2} {list of stuff} {code}
    // foreach var {list1 of stuff} {var2 var3} {list2 of stuff} {code}
    // dunno why this is giving me such trouble, it should work a lot like if -> elseif, ... -> else
    // technically the "var" can also be an arbitrary command, fun!
    // So how do we know when to stop matching clauses? Seems like a tough
    // thing to do for some reason (maybe because t-s isn't looking far enough ahead?)
    // Is this an external scanner thing? A precedence setting?
    foreach: $ => seq("foreach",
      $.foreach_clauses,
      $._word,
    ),

    // trying to hide or inline this causes conflicts oof
    foreach_clauses: $ => repeat1($.foreach_clause),

    foreach_clause: $ => seq($._word_simple, $._word_simple),

    global: $ => seq("global", repeat($._concat_word)),

    namespace: $ => seq('namespace', $._namespace_subcommand),

    _namespace_subcommand: $ => choice(
      seq("eval", $.word_list),
      $.word_list,
    ),

    try: $ => seq(
      "try",
      $._word,
      optional(seq(
        "on",
        "error",
        $.arguments,
        $._word,
      )),
      optional($.finally)
    ),

    finally: $=> seq('finally', $._word),

    _command: $ => choice(
      $._builtin,
      $.comment,
      $.command
    ),

    // commands are just _word's, okay
    command: $ => seq(
      field('name', $._word),
      optional(field('arguments', $.word_list)),
    ),

    word_list: $ => repeat1($._word),

    unpack: _ => '{*}',

    // Code to execute (kind of?)
    _word: $ => seq(
      optional($.unpack),
      choice(
        $.braced_word,
        $._concat_word,
      )
    ),

    // This was only used for the second clause of foreach statements (basically
    // what you're iterating over). Not sure if the single-item seq is
    // intentional, doesn't seem like it would matter.
    // Why can't this just be one of the other constructs? Seems like it's just
    // executing code.
    _word_simple: $ => choice(
      $.braced_word_simple,
      $._concat_word,
    ),

    _concat_word: $ => interleaved1(
      choice(
        $.escaped_character,
        $.command_substitution,
        $.simple_word,
        $.varname,
        $.quoted_word,
        $.variable_substitution,
      ),
      $.concat,
    ),

    _concat_word_expr: $ => interleaved1(
      choice(
        $.escaped_character,
        $.command_substitution,
        $.quoted_word,
        $.variable_substitution,
      ),
      $.concat,
    ),

    _ident: _ => token.immediate(/[a-zA-Z_][a-zA-Z0-9_]*/),

    // var name (should use this liberally for practicality rather than
    // allowing _word's everywhere
    // oh actually maybe this isn't that useful because of token.immediate in _ident?
    id: $ => seq(optional($._ns_delim), interleaved1($._ident, $._ns_delim)),

    array_index: $ => seq('(', $._concat_word, ')'),

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

    // This seems like it would actually be code to execute
    braced_word: $ => seq('{', choice($._commands, repeat($._terminator)), '}'),

    braced_word_simple: $ => seq('{',
      repeat(choice(
        $.braced_word_simple,
        $._concat_word,
        "\n", // still want?
      )),
    '}'),

    set: $ => seq("set",
    choice(
      // don't love this choice, _word is so flexible
      // I guess it's technically allowed, though (var names don't have to be literals, though I wish they did)
      $._word,
      // $.array_name
    ),
    optional($._word)),

    procedure: $ => seq(
      "proc",
      field('name', $._word),
      field('arguments', $.arguments),
      field('body', $._word)
    ),

    _argument_word: $ => choice($.simple_word, $.quoted_word, $.braced_word_simple),

    argument: $ => choice(
      field('name', $.simple_word),
      seq(
        '{',
         field('name', $.simple_word),
         optional(field('default', $._argument_word)),
         '}'
      )
    ),

    arguments: $ => choice(
      seq('{', repeat($.argument) , '}'),
      $.simple_word,
    ),

    _expr: $ => choice(
      seq("(", $._expr, ")"),
      seq($.simple_word, "(", optional(interleaved1($._expr, ",")), ")"),
      $.unary_expr,
      $.binop_expr,
      $.ternary_expr,
      $._concat_word_expr,
      $.int_literal,
      $.float_literal,
      $.bool_literal,
      // hmm this has some odd behavior where expr {} results in (expr_cmd (expr (braced_word_simple)))
      // This happens because we allow for a braceless expr body but then match braced_word_simple.
      // The issue goes away if we disallow braceless expr (best practice anyway).
      // I think (expr_cmd (braced_word_simple)) or (expr_cmd (expr)) would make more sense
      // expr {} isn't even a valid expr!
      // expr {} doesn't even work if you imagine you're calling in in braceless mode, so maybe we just ban that.
      // This also allows bad expressions like expr {any bare words} because the expression matches as a braced_word
      $.braced_word_simple,
    ),

    expr: $ => choice(
      seq('{', $._expr, '}'),
      $._expr,
    ),

    unary_expr: $ => prec.left(PREC.unary, seq(choice("-", "+", "~", "!"), $._expr)),

    binop_expr: $ => choice(
      prec.left(PREC.exp,          seq($._expr, "**",  $._expr)),

      prec.left(PREC.muldiv,       seq($._expr, "/",  $._expr)),
      prec.left(PREC.muldiv,       seq($._expr, "*",  $._expr)),
      prec.left(PREC.muldiv,       seq($._expr, "%",  $._expr)),
      prec.left(PREC.addsub,       seq($._expr, "+",  $._expr)),
      prec.left(PREC.addsub,       seq($._expr, "-",  $._expr)),

      prec.left(PREC.shift,        seq($._expr, "<<", $._expr)),
      prec.left(PREC.shift,        seq($._expr, ">>", $._expr)),

      prec.left(PREC.compare,      seq($._expr, ">",  $._expr)),
      prec.left(PREC.compare,      seq($._expr, "<",  $._expr)),
      prec.left(PREC.compare,      seq($._expr, ">=", $._expr)),
      prec.left(PREC.compare,      seq($._expr, "<=", $._expr)),

      prec.left(PREC.equal_bool,   seq($._expr, "==", $._expr)),
      prec.left(PREC.equal_bool,   seq($._expr, "!=", $._expr)),

      prec.left(PREC.equal_string, seq($._expr, "eq", $._expr)),
      prec.left(PREC.equal_string, seq($._expr, "ne", $._expr)),

      prec.left(PREC.contain,      seq($._expr, "in", choice($._concat_word, $.braced_word_simple))),
      prec.left(PREC.contain,      seq($._expr, "ni", choice($._concat_word, $.braced_word_simple))),

      prec.left(PREC.and_bit,      seq($._expr, "&", $._expr)),
      prec.left(PREC.xor_bit,      seq($._expr, "^", $._expr)),
      prec.left(PREC.or_bit,       seq($._expr, "|", $._expr)),

      prec.left(PREC.and_logical,  seq($._expr, "&&", $._expr)),
      prec.left(PREC.or_logical,   seq($._expr, "||", $._expr)),
    ),

    ternary_expr: $ => prec.left(PREC.ternary, seq($._expr, '?', $._expr, ':', $._expr)),

    elseif: $ => seq(
      "elseif",
      field('condition', $.expr),
      optional("then"),
      $._word,
    ),

    else: $ => seq(
      "else",
      $._word,
    ),

    conditional: $ => seq(
      "if",
      field('condition', $.expr),
      optional("then"),
      $._word,
      repeat($.elseif),
      optional($.else),
    ),

    catch: $ => seq(
      "catch",
      $._word,
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

    escaped_character: _ => /\\./,

    // token() bit seems useless, unless it's needed due to the prec wrapper?
    // this explains it kinda: https://github.com/tree-sitter/tree-sitter/issues/1087#issuecomment-833198651
    _quoted_word_content: _ => token(prec(-1, /[^$\\\[\]"]+/)),

    command_substitution: $ => seq('[', $._command, ']'),

    bool_literal: $ => /(((t)r?)u?)e?|((((f)a?)l?)s?)e?|on|(of)f?|((y)e?)s?|(n)o?/i,

    inf_literal: $ => token(infLiteral),
    nan_literal: $ => token(nanLiteral),
    int_literal: $ => token(intLiteral),
    float_literal: $ => token(floatLiteral),

    // token() unneeded afaik
    // basically seems to match just A-Za-z0-9_ but idk,
    // how does this differ from _ident? oh it's the lack of token.immediate
    simple_word: _ => token(/[^!$\s\\\[\]{}();"]+/),
  }

});
