const PREC = {
  or      : 10,
  and     : 20,
  compare : 30,
  plus    : 40,
  divide  : 50,
  times   : 60,
  ternary : 60,
}

const interleaved1 = (rule, delim) => seq(rule, repeat(seq(delim, rule)))

module.exports = grammar({
  name: 'tcl',

  word: $ => $.simple_word,

  externals: $ => [
    $.concat
  ],

  inline: $ => [
    $._commands,
    $._builtin,
    $._terminator,
    $._word,
  ],

  extras: $ => [
    /\s+/,
    /\\\r?\n/
  ],

  rules: {
    source_file: $ => $._commands,

    _commands: $ => seq(
      interleaved1($._command, $._terminator),
      optional($._terminator)
    ),

    _terminator: _ => choice('\n', ';'),

    comment: _ => /#[^\n]*/,

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
    ),

    while: $ => seq('while', $.expr, $._word),

    expr_cmd: $ => seq('expr', $.expr),

    foreach: $ => seq("foreach", $.arguments, $._word, $._word),

    global: $ => seq("global", repeat($._concat_word)),

    namespace: $ => seq('namespace', $.word_list),

    try: $ => seq(
      "try",
      $._word,
      optional(seq(
        "on",
        "error",
        $.arguments,
        $._word,
      ))
    ),

    _command: $ => choice(
      $._builtin,
      $.comment,
      $.command
    ),

    command: $ => seq(
      field('name', $._word),
      optional(field('arguments', $.word_list))
    ),

    word_list: $ => repeat1($._word),

    unpack: _ => '{*}',

    _word: $ => seq(
      optional($.unpack),
      choice(
        $.braced_word,
        $._concat_word,
      )
    ),

    _concat_word: $ => interleaved1(
      choice(
        $.command_substitution,
        $.simple_word,
        $.quoted_word,
        $.variable_substitution,
      ),
      $.concat,
    ),

    id: $=> repeat1(seq(optional($._ns_delim), $._ident)),

    _ns_delim: _ => "::",
    _ident: _ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    array_index: $ => seq('(', $.simple_word, ')'),

    variable_substitution: $ => seq(
      choice(
        seq('$', $.id),
        seq('${', /[^}]+/, '}'),
      ),
      optional($.array_index)
    ),

    // braced_content: $ => /[^{}]+/,
    // _braced_content: $ => repeat1(choice('\n', $._word)),

    braced_word: $ => seq('{',
      optional(choice(
        prec(3, $._commands),
        // prec(2, $.braced_word),
        // prec(1, $.braced_content)
      )),
    '}'),

    braced_word_simple: $ => seq('{',
      repeat(choice(
        $.braced_word_simple,
        $._concat_word,
      )),
    '}'),

    set: $ => seq("set", $._word, $._word),

    procedure: $ => seq(
      "proc",
      field('name', $._concat_word),
      field('arguments', $.arguments),
      field('body', $._word)
    ),

    _argument_word: $ => choice($.simple_word, $.quoted_word, $.braced_word),

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

    ternary_expr: $ => prec.left(PREC.ternary, seq($.expr, '?', $.expr, ':', $.expr)),

    expr: $ => choice(
      seq("{", $.expr, "}"),
      seq("(", $.expr, ")"),
      seq($.simple_word, "(", $.expr, ")"),
      $.unary_expr,
      $.binop_expr,
      $.ternary_expr,
      $._concat_word,
    ),

    unary_expr: $ => seq("!", $.expr),

    binop_expr: $ => choice(
      prec.left(PREC.plus,    seq($.expr, "+",  $.expr)),
      prec.left(PREC.plus,    seq($.expr, "-",  $.expr)),
      prec.left(PREC.divide,  seq($.expr, "/",  $.expr)),
      prec.left(PREC.times,   seq($.expr, "*",  $.expr)),
      prec.left(PREC.compare, seq($.expr, "eq", $.expr)),
      prec.left(PREC.compare, seq($.expr, "==", $.expr)),
      prec.left(PREC.compare, seq($.expr, "ne", $.expr)),
      prec.left(PREC.compare, seq($.expr, "!=", $.expr)),
      prec.left(PREC.compare, seq($.expr, "in", $.braced_word_simple)),
      prec.left(PREC.compare, seq($.expr, "ni", $.braced_word_simple)),
      prec.left(PREC.compare, seq($.expr, ">",  $.expr)),
      prec.left(PREC.compare, seq($.expr, "<",  $.expr)),
      prec.left(PREC.compare, seq($.expr, ">=", $.expr)),
      prec.left(PREC.compare, seq($.expr, "<=", $.expr)),
      prec.left(PREC.and,     seq($.expr, "&&", $.expr)),
      prec.left(PREC.or,      seq($.expr, "||", $.expr)),
    ),

    elseif: $ => seq(
      "elseif",
      field('condition', $.expr),
      $._word,
    ),

    else: $ => seq(
      "else",
      $._word,
    ),

    conditional: $ => seq(
      "if",
      field('condition', $.expr),
      $._word,
      repeat($.elseif),
      optional($.else),
    ),

    quoted_word: $ => seq(
      '"',
      repeat(
        choice(
          $._quoted_word_content,
          $.variable_substitution,
          $.command_substitution,
          $.escaped_character,
        ),
      ),
      '"',
    ),

    escaped_character: _ => /\\./,

    _quoted_word_content: _ => /[^$\\\[\]"]+/,

    command_substitution: $ => seq('[', $._command, ']'),

    simple_word: _ => token(/[^!$\s\\\[\]{}();"]+/),
  }
});

