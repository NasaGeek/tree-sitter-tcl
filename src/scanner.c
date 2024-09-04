#include "tree_sitter/parser.h"
#include <wctype.h>
#ifndef __wasm__
#include <stdarg.h>
#include <stdio.h>
#endif

enum TokenType {
  CONCAT,
  NS_DELIM,
  COMMAND_SEPARATOR,
  // Not used in the grammar, but used in the external scanner to check for error state.
  // This relies on the tree-sitter behavior that when an error is encountered the external
  // scanner is called with all symbols marked as valid.
  ERROR,
};

#define DEBUG 0

static bool myprintf(const char *format, ...) {
#if DEBUG
#ifndef __wasm__
    va_list args;
    int result;
    va_start(args, format);
    result = vprintf(format, args);
    va_end(args);
    return result;
#else
    return 1;
#endif
#endif
    return 1;
}

static bool is_bare_word(int32_t chr) {
    return iswalnum(chr) || chr == '_';
}

static bool is_simple_word(TSLexer *lexer, int32_t chr) {
    // Matches simple_word definition in grammar.js
    return (!iswspace(chr) &&
            !lexer->eof(lexer) &&
            chr != '\\' &&
            chr != '[' &&
            chr != ']' &&
            chr != '$' &&
            chr != '{' &&
            chr != '}' &&
            chr != '(' &&
            chr != ')' &&
            chr != ';' &&
            chr != '"');
}

static bool is_tcl_whitespace(int32_t chr) {
    return iswspace(chr) && chr != '\n';
}

static bool is_tcl_separator(int32_t chr) {
    return chr == '\n' || chr == ';';
}

static bool is_concat_valid(TSLexer *lexer, const bool *valid_symbols) {
    /* Don't yet have a good answer to how we allow stuff like ]/{/}/" in strings
     * that didn't start with " or {
     */
    if (valid_symbols[CONCAT]) {
        if (is_simple_word(lexer, lexer->lookahead) ||
                lexer->lookahead == '$' ||
                lexer->lookahead == '[') {
            return true;
        } else if (lexer->lookahead == '\\') {
            lexer->mark_end(lexer);
            lexer->advance(lexer, false);
            return lexer->lookahead != '\n';
        }
    }
    return false;
}

static bool has_followup_command(TSLexer *lexer) {
    return !(lexer->lookahead == ']' || lexer->lookahead == '}' || lexer->eof(lexer));
}

static bool scan_ns_delim(TSLexer *lexer) {
  if (lexer->lookahead == ':') {
    lexer->advance(lexer, false);
    if (lexer->lookahead == ':') {
      lexer->advance(lexer, false);
      if (is_bare_word(lexer->lookahead)) {
        lexer->result_symbol = NS_DELIM;
        return true;
      }
    }
  }
  return false;
}

void *tree_sitter_tcl_external_scanner_create() {
  return NULL;
}

bool tree_sitter_tcl_external_scanner_scan(void *payload, TSLexer *lexer,
                                          const bool *valid_symbols) {
  myprintf("Column: %d, lookahead: '%c', symbols: %d %d %d %d\n", lexer->get_column(lexer), lexer->lookahead, valid_symbols[CONCAT], valid_symbols[NS_DELIM], valid_symbols[COMMAND_SEPARATOR], valid_symbols[ERROR]);
  if (valid_symbols[ERROR]) {
      return false;
  }
  if (valid_symbols[NS_DELIM] && scan_ns_delim(lexer)) {
    myprintf("We picked ns\n");
    return true;
  }

  if (valid_symbols[COMMAND_SEPARATOR] && (is_tcl_whitespace(lexer->lookahead) || is_tcl_separator(lexer->lookahead))) {
      bool saw_separator = false;
      lexer->mark_end(lexer);
      do {
          while (is_tcl_whitespace(lexer->lookahead)) {
              lexer->advance(lexer, false);
          }
          while (is_tcl_separator(lexer->lookahead)) {
              lexer->advance(lexer, false);
              if (!saw_separator) {
                  lexer->mark_end(lexer);
                  saw_separator = true;
              }
          }
      } while (is_tcl_whitespace(lexer->lookahead));
      if (saw_separator && has_followup_command(lexer)) {
          myprintf("We picked separator\n");
          lexer->result_symbol = COMMAND_SEPARATOR;
      }
      myprintf("Seemed like separator but picked nothing\n");
      return saw_separator && has_followup_command(lexer);
  }

  if (is_concat_valid(lexer, valid_symbols)) {
    myprintf("We picked concat\n");
    lexer->result_symbol = CONCAT;
    return true;
  }

  myprintf("We picked neither\n");
  return false;
}

unsigned tree_sitter_tcl_external_scanner_serialize(void *payload, char *state) {
  return 0;
}

void tree_sitter_tcl_external_scanner_deserialize(void *payload, const char *state, unsigned length){ }

void tree_sitter_tcl_external_scanner_destroy(void *payload) {}
