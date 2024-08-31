#include "tree_sitter/parser.h"
#include <wctype.h>
#ifndef __wasm__
#include <stdarg.h>
#include <stdio.h>
#endif

enum TokenType {
  CONCAT,
  NS_DELIM,
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

static bool is_concat_valid(TSLexer *lexer, const bool *valid_symbols) {
  return valid_symbols[CONCAT] && (
    is_bare_word(lexer->lookahead) ||
    lexer->lookahead == '$' ||
    lexer->lookahead == '[' ||
    lexer->lookahead == '\\'
  );
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
  myprintf("Column: %d, lookahead: '%c', symbols: %d %d %d\n", lexer->get_column(lexer), lexer->lookahead, valid_symbols[CONCAT], valid_symbols[NS_DELIM], valid_symbols[ERROR]);
  if (valid_symbols[ERROR]) {
      return false;
  }
  if (valid_symbols[NS_DELIM] && scan_ns_delim(lexer)) {
    myprintf("We picked ns\n");
    return true;
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
