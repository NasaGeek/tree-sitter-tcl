// General "issues" that persist:
// hardcoded {} in various spots which should actually be interchangeable with ""
//  except it's a thorny issue, so probably will just force braces for most places
// poor bareword (simple_word) special character handling
//  not sure this is worth fixing; just quote your stuff!
// unbraced exprs and switches unsupported
//  This has come up a few times in real code but only with single-arg exprs
// exprs generally need whitespace between operators/operands
//  In reality this doesn't come up much because it only affects expression with
//  a bunch of literals

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



const expr_seq = (seqfn, suffix) => {
  // Enables the creation of multiple "subtrees" of the grammar, one supporting
  // exprs with newlines basically everywhere, and the other supporting only
  // the usual extras (enabling exprs inside and outside {})
  const _expr = "_expr"+suffix;
  const unary_expr = "unary_expr"+suffix;
  const binop_expr = "binop_expr"+suffix;
  const func_call = "func_call"+suffix;
  const func_args = "func_args"+suffix;
  const ternary_expr = "ternary_expr"+suffix;

  return {
    // expr in general is such a spin on Tcl's usual syntax that it's quite
    // difficult to support well alongside many of Tcl's other idiosyncracies.
    [_expr]: $ => choice(
      seqfn("(", $[_expr], ")"),
      alias($[unary_expr], $.unary_expr),
      alias($[binop_expr], $.binop_expr),
      alias($[ternary_expr], $.ternary_expr),
      alias($[func_call], $.func_call),
      $.braced_word,
      $._non_seq_expr,
    ),

    // Splitting out expr elements that don't use seq so I can reuse this
    // for both expr-based and non-expr-based expressions.
    _non_seq_expr: $ => choice(
      $.int_literal,
      $.float_literal,
      $.bool_literal,
      $._concat_word_expr,
    ),

    [func_call]: $ => seqfn(
        field("name", $.expr_function_name),
        "(",
        field("args", optional(alias($[func_args], $.func_args))),
        ")"
      ),

    [func_args]: $ => prec.left(interleaved_seq1($[_expr], ",", seqfn)),

    [unary_expr]: $ => prec.left(PREC.unary, seqfn(choice("-", "+", "~", "!"), $[_expr])),

    [binop_expr]: $ => choice(
      prec.left(PREC.exp,          seqfn($[_expr], "**",  $[_expr])),

      prec.left(PREC.muldiv,       seqfn($[_expr], "/",  $[_expr])),
      prec.left(PREC.muldiv,       seqfn($[_expr], "*",  $[_expr])),
      prec.left(PREC.muldiv,       seqfn($[_expr], "%",  $[_expr])),
      prec.left(PREC.addsub,       seqfn($[_expr], "+",  $[_expr])),
      prec.left(PREC.addsub,       seqfn($[_expr], "-",  $[_expr])),

      prec.left(PREC.shift,        seqfn($[_expr], "<<", $[_expr])),
      prec.left(PREC.shift,        seqfn($[_expr], ">>", $[_expr])),

      prec.left(PREC.compare,      seqfn($[_expr], ">",  $[_expr])),
      prec.left(PREC.compare,      seqfn($[_expr], "<",  $[_expr])),
      prec.left(PREC.compare,      seqfn($[_expr], ">=", $[_expr])),
      prec.left(PREC.compare,      seqfn($[_expr], "<=", $[_expr])),

      prec.left(PREC.equal_bool,   seqfn($[_expr], "==", $[_expr])),
      prec.left(PREC.equal_bool,   seqfn($[_expr], "!=", $[_expr])),

      prec.left(PREC.equal_string, seqfn($[_expr], "eq", $[_expr])),
      prec.left(PREC.equal_string, seqfn($[_expr], "ne", $[_expr])),

      prec.left(PREC.contain,      seqfn($[_expr], "in", $._word)),
      prec.left(PREC.contain,      seqfn($[_expr], "ni", $._word)),

      prec.left(PREC.and_bit,      seqfn($[_expr], "&", $[_expr])),
      prec.left(PREC.xor_bit,      seqfn($[_expr], "^", $[_expr])),
      prec.left(PREC.or_bit,       seqfn($[_expr], "|", $[_expr])),

      prec.left(PREC.and_logical,  seqfn($[_expr], "&&", $[_expr])),
      prec.left(PREC.or_logical,   seqfn($[_expr], "||", $[_expr])),
    ),

    [ternary_expr]: $ => prec.left(PREC.ternary, seqfn($[_expr], '?', $[_expr], ':', $[_expr])),
  }
}

// https://www.tcl-lang.org/cgi-bin/tct/tip/407.html#:~:text=Tcl%27s%20source%20code.-,String%20Representation%20of%20Lists,-The%20routines%20%5Bin
const gap = token(/([ \t\v\f\r]|\\\r?\n)+/)

const intergappednl1 = (rule) => interleaved1(rule, repeat1(choice(gap, '\n')))
const intergappednl = (rule) => optional(intergappednl1(rule))
const intergapped1 = (rule) => interleaved1(rule, gap)
const intergapped = (rule) => optional(intergapped1(rule))
const interleaved1 = (rule, delim) => seq(rule, repeat(seq(delim, rule)))
const interleaved = (rule, delim) => optional(interleaved1(rule, delim))
// interleaved1 but accepting a custom seq function
const interleaved_seq1 = (rule, delim, seqfn) => seqfn(rule, repeat(seqfn(delim, rule)))

// Helper for rules within which whitespace/newlines are irrelevant
// (essentially adding them as faux-extras). Note that this allows for _no_
// whitespace (making it primarily useful for expr). No need to implement this
// in term of seqdelim since the delims can collapse to blank.
const seqnl = (...rules) => seq(...rules.flatMap(e => [repeat(choice("\n", gap)), e]).slice(1))


// Helper for sequences that require some delimiter between their elements.
// Intelligently handles optional elements where the leading delimiter should
// be tucked inside the optional().
const seqdelim = (delim, ...rules) => {
  let result = rules.flatMap(e => {
    // This is what optionals are turned into
    if (e.type === "CHOICE" && e.members?.length === 2 && e.members[1].type == 'BLANK') {
      return [optional(seq(delim, e.members[0]))]
    } else if (e.type == "REPEAT") {
      return [repeat(seq(delim, e.content))]
    } else {
      return [delim, e]
    }
  })
  // Because we tuck the delim into certain rules, it won't necessarily always
  // be first. I'm not actually certain that this works correctly.
  if (result[0] == delim) {
    result = result.slice(1)
  } else {
    console.log("We didn't trim")
  }
  return seq(...result)
}

const seqgap = (...rules) => seqdelim(gap, ...rules)
// Similar to seqnl, but at least some whitespace/newline is required vs being
// completely irrelevant, useful for various constructs in {}
const seqgapnl = (...rules) => seqdelim(repeat1(choice('\n', gap)), ...rules)

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
    // looks for if next token is alphanum, _, $, [, or \(not-whitespace).
    // Covers separation of tokens that aren't whitespace-separated, think of
    // cases like [func][func] or a$a.
    // May or may not want to end up hiding this.
    $.concat,
    // looks for :: followed by alpha
    $._ns_delim,
    $._command_separator,
    // Checks whether a $ should indicate variable substitution or if it's just
    // a literal
    $._varsub_prefix,
    // Not used in the grammar, but used in the external scanner to check for error state.
    // This relies on the tree-sitter behavior that when an error is encountered the external
    // scanner is called with all symobls marked as valid.
    $.error,
  ],

  inline: $ => [
    $._commands,
    $._builtin,
    $.terminator,
    $.termgap,
    $.gap,
    $._word,
  ],

  extras: _ => [],

  conflicts: $ => [
    [$.switch_body],
    [$.foreach_clauses],

    [$.binop_expr_nl, $.ternary_expr_nl],
    [$.func_call_nl],
    [$.expr_cmd],

    [$.command],
    [$.set],
    [$.try],
    [$._word_list],
    [$._word_eval_list],
    [$.conditional],
    [$.string_cmd],
    [$.global],
    [$.catch],
    [$.arguments],
    [$._argument_content],
    [$._nested_raw_braces],
    [$._nested_raw_quotes],
    [$._nested_braced_list],
    [$._nested_quoted_list],
    [$._tl_nested_quoted_list],
  ],

  rules: {
    // https://wiki.tcl-lang.org/page/the+empty+command
    source_file: $ => choice($._commands, optional($.termgap)),

    // https://www.tcl-lang.org/man/tcl8.6/TclCmd/Tcl.htm#M5
    _commands: $ => seq(
      optional($.termgap),
      $._command,
      repeat(
        seq(
          $._command_separator,
          optional($.termgap),
          $._command
        )
      ),
      optional($.termgap),
    ),

    terminator: _ => choice('\n', ';'),

    gap: _ => gap,

    termgap: $ => repeat1(
        choice(
          $.terminator,
          $.gap,
        )
    ),

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
      $.switch,
      $.expr_cmd,
      $.while,
      $.catch,
      alias($.string_cmd, $.command),
    ),

    string_cmd: $ => seqgap(
        field("name", alias("string", $.simple_word)),
        field("arguments",
          choice(
            // This acts as an identity function. We can use it in code to force
            // interpretation of an arg as a $.script. I know, clunky. Not my
            // favorite solution to all this but it's all I've got.
            seqgap(alias("cat", $.simple_word), optional($._word_eval_list)),
            $._word_list,
          ),
        ),
      ),

    while: $ => seqgap('while', $.expr, $.script),

    // We technically support multi-arg expr's, but make no attempt to parse
    // them as such (except for the first argument)
    expr_cmd: $ => seqgap('expr', $.expr, optional($._word_list)),

    for: $ => seqgap("for",
      $.script,
      $.expr,
      $.script,
      $.script,
    ),

    // https://www.tcl.tk/man/tcl/TclCmd/foreach.htm
    // This unfortunately can be quite sensitive to parse errors in the body
    // causing the foreach to just start stuffing things into foreach_clauses
    // as braced_word instead of trying to recover somehow.
    foreach: $ => seqgap("foreach",
      $.foreach_clauses,
      $.script,
    ),

    foreach_clauses: $ => intergapped1($.foreach_clause),

    foreach_clause: $ => seqgap($.literal_list, $._word),

    // https://www.tcl.tk/man/tcl/TclCmd/switch.htm
    switch: $ => seqgap("switch",
      field("arguments", intergapped1($._word)),
      // Maybe this is too complicated for poor tree-sitter. It used to work
      // alright, but I broke it at some point.
      // field("flags", intergapped($._word)),
      // field("pattern", $._word),
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
        intergappednl($._inner_switch),
        "}"
      ),
    ),

    _inner_switch: $ => seqgapnl(
      $.raw_word,
      // Ehh not totally sold on the aliasing
      choice($.script, alias('-', $.script)),
    ),

    // evaluated,
    // terminator-delimited commands,
    // surrounded by "", {}, or nothing (though in the nothing case it must not have whitespace)
    // Name is slighty awkward, since we don't _know_ things will be eval'ed as
    // scripts. We could call it maybe_script?
    // Also consider hiding this, it's just a wrapper.
    script: $ => choice(
      $._concat_word,
      seq("{", optional($._script_body), "}"),
      // Broadly applicable quotes are really tough, need to ban them in
      // certain constructs or otherwise figure out how to deal with them
      // (custom lexing?)
      // seqnl('"', choice($._commands, repeat($.terminator)), '"'),
    ),

    _script_body: $ => choice($._commands, $.termgap),

    global: $ => seqgap("global", intergapped($._word)),

    namespace: $ => seqgap('namespace', $._namespace_subcommand),

    _namespace_subcommand: $ => choice(
      seqgap("eval", $._word_eval_list),
      $._word_list,
    ),

    try: $ => seqgap(
      "try",
      $.script,
      repeat(choice(
        seqgap(
          "on",
          choice(
            "ok", "error", "return", "break", "continue", /[0-4]/
          ),
          $.literal_list,
          $.script,
        ),
        seqgap(
          "trap",
          $._word,
          $.literal_list,
          $.script,
        )
      )),
      optional($.finally),
    ),

    finally: $=> seqgap('finally', $.script),

    _command: $ => choice(
      $._builtin,
      $.comment,
      $.command
    ),

    // commands are just _word's, okay
    command: $ => seqgap(
      field('name', $._word),
      optional(field('arguments', $._word_list)),
    ),

    // https://www.tcl-lang.org/man/tcl8.6/TclCmd/Tcl.htm#M9
    unpack: _ => '{*}',

    _word_eval_list: $ => intergapped1($._word_eval),

    // A word that might possibly be evaluated as code
    _word_eval: $ => seq(
      optional($.unpack),
      $.script,
    ),

    _word_list: $ => intergapped1($._word),

    // A word that we don't expect to be evaluated as code. This is used by
    // default for most arguments, as the alternative is attempting to parse
    // literally everything in any string which can fall over quickly. Instead
    // we follow a whitelist approach of whether words should be treated as
    // code, along with the `string cat` escape hatch.
    _word: $ => seq(
      optional($.unpack),
      choice($.braced_word, $._concat_word, $.quoted_word)
    ),

    // Might end up useful for arbitrary quoted stuff...
    // _concat_word_noquote: $ => interleaved1(
    //   choice(
    //     $.escape_sequence,
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
    // FIXME: This splits on stuff like a"a" or a{a} which should actually end
    // up as 1 token (probably needs handling in the scanner). The word just
    // can't start with " or {.
    _concat_word: $ => interleaved1(
      choice(
        $.escape_sequence,
        $.command_substitution,
        $.variable_substitution,
        $.simple_word,
        $.array_name,
      ),
      $.concat,
    ),

    _concat_word_array_index: $ => repeat1(
      choice(
        $.escape_sequence,
        $.command_substitution,
        $.variable_substitution,
        $.array_index_word,
      ),
    ),

    // bare words are a no-no inside of expr's, but quoted is okay. The rules
    // in expr's are generally much looser, too, meaning we don't need the
    // concat check.
    _concat_word_expr: $ => repeat1(
      choice(
        $.escape_sequence,
        $.command_substitution,
        $.quoted_word,
        $.variable_substitution,
      ),
    ),

    // Specifically (only?) useful for variable substitution due to
    // token.immediate
    _ident: _ => token.immediate(prec(2, /[a-zA-Z0-9_]+/)),

    id: $ => choice(
      // FIXME: support $:: and $::a::. As written this causes variable subs
      // to split between variable sub and simple_word.
      seq(optional($._ns_delim), interleaved1($._ident, $._ns_delim)), //, optional($._ns_delim)),
      // Yes you can have a variable named {} (accessed with $:: or $(index) if
      // it's an array). This breaks recognition of other namespaced variables,
      // though
      // $._ns_delim,
    ),

    // token.immediate breaks usage of () in bare words (maybe that's okay?)
    // e.g. `puts (string)` should work fine, treat (string) as a string.
    // maybe it makes sense to include () in simple_word or parse arrays in
    // scanner.c?
    // We want token.immediate so abc (xyz) isn't treated as an array, though.

    // An array index can contain almost any non-whitespace.
    // Unfortunately since whitespace is an extra we still allow it incorrectly.
    // Just another thing for the scanner to handle :)
    // Also arr(...$) is treated as a literal $ by Tcl whereas we throw an error
    _array_index: $ => seq(
      '(',
      optional($._concat_word_array_index),
      ')',
    ),

    // cheating here a bit by restricting what an array name can be. If we want
    // stuff like arr(a)(b) or (a) (yes the array name can be empty) to work
    // then will probably need to implement it in the scanner (along with
    // removing the () exclusion from simple_word).
    // Also might have to remove _array_index's token.immediate
    array_name: $ => seq($.simple_word, field('index', $._array_index)),

    // Improving this with optional($.id) means we lose the node altogether which
    // I don't like (can't we have an empty node instead?)
    array_ref: $ => seq($.id, field('index', $._array_index)),

    variable_substitution: $ => seq(
      $._varsub_prefix,
      token(prec(2, '$')),
      choice(
        $.id,
        // FIXME: Missing parsing of array ref in here
        seq('{', /[^}]+/, '}'),
        $.array_ref,
      )
    ),

    set: $ => seqgap("set",
      $._word,
      optional($._word)),

    procedure: $ => seqgap(
      "proc",
      field('name', $._word),
      field('arguments', $.arguments),
      field('body', $.script)
    ),

    arguments: $ => choice(
      seqnl('{', intergapped($.argument), '}'),
      $._concat_word,
    ),

    argument: $ => choice(
      field('name', $.simple_word),
      seqnl('{', $._argument_content, '}')
    ),

    _argument_content: $ => seqgapnl(
        field('name', $.raw_word),
        optional(field('default', $.raw_word)),
    ),

    // This is specifically used for expressions that aren't `expr`, since
    // everywhere else requires it to be one word. This is also intentionally
    // restricted to match a subset of unbraced valid expressions due to the
    // current $._expr approach allowing arbitrary whitespace. If people want
    // max flexibility, just wrap in {}
    expr: $ => choice(
      // prec disambiguates from braced_word
      seqnl(token(prec(1, '{')), $._expr_nl, '}'),
      $._expr,
    ),

    ...expr_seq(seqnl, '_nl'),
    ...expr_seq(seq, ''),

    conditional: $ => seqgap(
      "if",
      field('condition', $.expr),
      optional("then"),
      $.script,
      intergapped($.elseif),
      optional($.else),
    ),

    elseif: $ => seqgap(
      "elseif",
      field('condition', $.expr),
      optional("then"),
      $.script,
    ),

    else: $ => seqgap(
      "else",
      $.script,
    ),

    catch: $ => seqgap(
      "catch",
      $.script,
      optional(seqgap($._word, optional($._word)))
    ),

    quoted_word: $ => seq(
      '"',
      repeat(choice(
        $.variable_substitution,
        $._quoted_word_content,
        $.command_substitution,
        $.escape_sequence,
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
    // This can cause problems when we get into an error state because it's so
    // flexible, resulting in large chunks of code just becoming braced_word.
    // Not sure how to get better error recovery without just removing this or
    // making it match less.
    braced_word: $ => seq('{', repeat(choice($._nested_braces, $._braced_word_contents)), '}'),


    _raw_word_contents: _ => token(/[^{}"\s]+/),

    _nested_raw_braces: $ => seqnl('{', repeat(choice('\n', $.gap, $._nested_raw_braces, $._nested_raw_quotes, $._raw_word_contents)), '}'),
    _nested_raw_quotes: $ => seqnl('"', repeat(choice('\n', $.gap, $._nested_raw_braces, $._raw_word_contents)), '"'),

    // This should only be used when we're already inside of braces, since none
    // of the contents are going to be substitutable. This is basically just
    // braced_word but we're already inside the braces and restricted in how
    // spaces can be placed (like switch cases and arguments).
    // FIXME: doesn't allow stuff like a" (just like everywhere else)
    raw_word: $ => choice(
      // Don't expect this to be bulletproof. It's just an interim solution until
      // I figure out proper literal parsing throughout the grammar.
      $._nested_raw_braces,
      $._nested_raw_quotes,
      seq($._raw_word_contents, repeat(choice($._nested_raw_braces, $._raw_word_contents))),
    ),

    list_item: _ => token(prec(-1, /[^{}"\s]+/)),

    _tl_nested_quoted_list: $ => seqnl('"',
      repeat(
        choice(
          '\n',
          $.gap,
          // Substitution is permitted only at top level
          $._concat_word,
          alias($._nested_braced_list, $.literal_list),
          $.list_item
        )
      ),
      '"'),

    _nested_quoted_list: $ => seqnl('"',
      repeat(
        choice(
          '\n',
          $.gap,
          alias($._nested_braced_list, $.literal_list),
          $.list_item
        )
      ),
      '"'),

    _nested_braced_list: $ => seqnl('{',
      repeat(
        choice(
          '\n',
          $.gap,
          alias($._nested_braced_list, $.literal_list),
          alias($._nested_quoted_list, $.literal_list),
          $.list_item
        )
      ),
      '}'),

    // Covers cases where a literal can be specified and is interpreted as a list,
    // like foreach vars or trap/on vars.
    // Feels very similar to raw_word, but is not opaque within.
    // Nests infinitely, when realistically we probably only ever care about
    // the first level.
    literal_list: $ => choice(
      $._tl_nested_quoted_list, $._nested_braced_list, $.list_item
    ),

    escape_sequence: _ => token(seq(
      '\\',
      choice(
        /./, // note this won't match newlines
        /[0-7]{1,3}/,
        /x[0-9a-fA-F]{1,2}/,
        /u[0-9a-fA-F]{1,4}/,
        /U[0-9a-fA-F]{1,8}/,
      ),
    )),

    // https://github.com/tree-sitter/tree-sitter/issues/1087#issuecomment-833198651
    _quoted_word_content: _ => token(prec(-1, /([^$\\\[\]"]|\\\r?\n)+|\$/)),

    command_substitution: $ => seq('[', optional($._script_body), ']'),

    // lol Tcl you cray
    bool_literal: _ => token(prec.dynamic(-2, /(((t)r?)u?)e?|((((f)a?)l?)s?)e?|on|(of)f?|((y)e?)s?|(n)o?/i)),

    int_literal: _ => token(prec.dynamic(-2, intLiteral)),
    float_literal: _ => token(prec.dynamic(-2, floatLiteral)),

    // I'd kind of like to remove () from the exclusion for matching array names,
    // but there are knock-on effects like degraded recognition of function calls in exprs
    simple_word: _ => token(prec.dynamic(-1, /[^\s\\\[\]{}()$;"]+|\$/)),

    // Functions in exprs are actually slightly more restricted bare words (no
    // leading _ for arbitrary reasons: https://github.com/tcltk/tcl/blob/core-8-6-14/generic/tclCompExpr.c#L2063-L2065, sigh...)
    expr_function_name: _ => token(/[A-Za-z0-9][A-Za-z0-9_]*/),

    // True barewords matching. Doesn't match $/[ because this is expected to be
    // used alongside _concat_word. We also exclude ) as a hack because otherwise
    // we can't recognize the end of an array reference.
    // And as yet another hack we have ( to allow for nested array accesses.
    array_index_word: _ => token(/[^\s\\\[$;()]+/),
  }

  });
