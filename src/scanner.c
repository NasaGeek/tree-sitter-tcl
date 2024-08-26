#include "tree_sitter/parser.h"
#include <wctype.h>

enum TokenType {
  CONCAT,
  NS_DELIM,
  // Not used in the grammar, but used in the external scanner to check for error state.
  // This relies on the tree-sitter behavior that when an error is encountered the external
  // scanner is called with all symbols marked as valid.
  ERROR,
};

static bool is_eof(TSLexer *lexer) {
  return lexer->lookahead == 0;
}

static bool is_concat_valid(TSLexer *lexer, const bool *valid_symbols) {
  return valid_symbols[CONCAT] && (
    iswalpha(lexer->lookahead) ||
    lexer->lookahead == '$' ||
    lexer->lookahead == '[' ||
    lexer->lookahead == '_'
  );
  // return valid_symbols[CONCAT] && !(
  //         is_eof(lexer) ||
  //         iswspace(lexer->lookahead) ||
  //         lexer->lookahead == ']' ||
  //         lexer->lookahead == '$' ||
  //         lexer->lookahead == ')' ||
  //         lexer->lookahead == '}'
  //         );
}

static bool scan_ns_delim(TSLexer *lexer) {
  if (lexer->lookahead == ':') {
    lexer->advance(lexer, false);
    if (lexer->lookahead == ':') {
      lexer->advance(lexer, false);
      if (iswalpha(lexer->lookahead)) {
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
  if (valid_symbols[ERROR]) {
      return false;
  }
  if (valid_symbols[NS_DELIM] && scan_ns_delim(lexer)) {
    return true;
  }

  if (is_concat_valid(lexer, valid_symbols)) {
    lexer->result_symbol = CONCAT;
    return true;
  }

  return false;
}

unsigned tree_sitter_tcl_external_scanner_serialize(void *payload, char *state) {
  return 0;
}

void tree_sitter_tcl_external_scanner_deserialize(void *payload, const char *state, unsigned length){ }

void tree_sitter_tcl_external_scanner_destroy(void *payload) {}
