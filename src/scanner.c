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
    VARSUB_PREFIX,
    GAP,
    EXPR_START,
    EXPR_END,
    CMDSUB_START,
    CMDSUB_END,
    NEWLINE,
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

typedef struct {
    int8_t expr_depth;
    int8_t cmdsub_depth;
} Scanner;

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
                return true;
            }
        }
    }
    return false;
}

static bool is_ns_separator(TSLexer *lexer) {
    if (lexer->lookahead == ':') {
        lexer->advance(lexer, false);
        if (lexer->lookahead == ':') {
            lexer->advance(lexer, false);
            while (lexer->lookahead == ':') {
                // https://www.tcl-lang.org/man/tcl8.6/TclCmd/namespace.htm#:~:text=two%20or%20more%20colons%20are%20treated%20as%20a%20namespace%20separator
                lexer->advance(lexer, false);
            }
            return true;
        }
    }
    return false;
}

static bool scan_variable_substitution(TSLexer *lexer) {
    // Not totally sure if I should be eating whitespace here (why not elsewhere, too?),
    // but it fixes expressions like `expr { $a}` and `puts " $a"`
    while (is_tcl_whitespace(lexer->lookahead)) {
        lexer->advance(lexer, true);
    }
    if (lexer->lookahead == '$') {
        lexer->mark_end(lexer);
        lexer->advance(lexer, false);
        return lexer->lookahead == '{' ||
            // Empty array name
            lexer->lookahead == '(' ||
            is_bare_word(lexer->lookahead) ||
            // Advances lexer, must go last
            is_ns_separator(lexer);
    }
    return false;
}

static bool scan_gap(TSLexer *lexer) {
    int32_t chr = lexer->lookahead;
    bool saw_gap = false;
    if (is_tcl_whitespace(lexer->lookahead)) {
        saw_gap = true;
        lexer->advance(lexer, false);
        // This will miss any escaped newlines, but that's okay because they're
        // extras anyway and we already saw our required gap
        while (is_tcl_whitespace(lexer->lookahead)) {
            lexer->advance(lexer, false);
        }
    } else if (chr == '\\') {
        lexer->mark_end(lexer);
        lexer->advance(lexer, false);
        // Probably need better checking that includes \r? along with several other spots...
        saw_gap = lexer->lookahead == '\n';
        if (saw_gap) {
            lexer->advance(lexer, false);
            lexer->mark_end(lexer);
        }
    }
    return saw_gap;
}

static bool is_command_end(TSLexer *lexer) {
    int32_t chr = lexer->lookahead;
    // TODO: quotes should go here, too, but they'll need special tracking to
    // identify closing vs opening quotes
    return chr == ';' || chr == '\n' || chr == ']' || chr == '}';
}

bool tree_sitter_tcl_external_scanner_scan(void *payload, TSLexer *lexer,
        const bool *valid_symbols) {
    myprintf("Column: %d, lookahead: '%c', symbols: %d %d %d %d %d %d %d %d %d %d %d\n",
            lexer->get_column(lexer),
            lexer->lookahead,
            valid_symbols[CONCAT],
            valid_symbols[NS_DELIM],
            valid_symbols[COMMAND_SEPARATOR],
            valid_symbols[VARSUB_PREFIX],
            valid_symbols[GAP],
            valid_symbols[EXPR_START],
            valid_symbols[EXPR_END],
            valid_symbols[CMDSUB_START],
            valid_symbols[CMDSUB_END],
            valid_symbols[NEWLINE],
            valid_symbols[ERROR]);

    if (valid_symbols[ERROR]) {
        return false;
    }

    Scanner *scanner = (Scanner*) payload;

    if (valid_symbols[EXPR_START]) {
        scanner->expr_depth++;
        lexer->result_symbol = EXPR_START;
        myprintf("expr_start\n");
        return true;
    }

    if (valid_symbols[CMDSUB_START]) {
        scanner->cmdsub_depth++;
        lexer->result_symbol = CMDSUB_START;
        myprintf("cmdsub_start\n");
        return true;
    }

    if (valid_symbols[CMDSUB_END]) {
        scanner->cmdsub_depth--;
        lexer->result_symbol = CMDSUB_END;
        return true;
    }

    if (valid_symbols[EXPR_END]) {
        scanner->expr_depth--;
        lexer->result_symbol = EXPR_END;
        return true;
    }


    if (lexer->lookahead == '\n' && !valid_symbols[NEWLINE] && scanner->expr_depth == 0) {
        // This seems extremely brittle...
        // Trying to avoid treating newlines as extras except when we're inside a
        // braced expr.
        // Placement is tricky, needs to go after any zero-length matches possibly.
        // Consider merging with the very similar handling of NEWLINE below.
        lexer->result_symbol = NEWLINE;
        return true;
    }

    if (valid_symbols[GAP]) {
        if (scanner->cmdsub_depth >= scanner->expr_depth && scan_gap(lexer)) {
            // This might end up too fragile and it'd be better to just
            // allow optional gap after args
            if (!is_command_end(lexer)) {
                lexer->result_symbol = GAP;
                myprintf("we picked gap\n");
                return true;
            }
        }
    }

    if (valid_symbols[NS_DELIM] && is_ns_separator(lexer)) {
        myprintf("We picked ns\n");
        lexer->result_symbol = NS_DELIM;
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
            return true;
        }
        myprintf("Seemed like separator but picked nothing\n");
        return false;
    }

    if (valid_symbols[NEWLINE] && scanner->expr_depth == 0 && lexer->lookahead == '\n') {
        // Treat it as an extra only inside exprs
        lexer->advance(lexer, false);
        lexer->result_symbol = NEWLINE;
        myprintf("newline\n");
        return true;
    }

    if (valid_symbols[VARSUB_PREFIX] && scan_variable_substitution(lexer)) {
        myprintf("We picked varsub\n");
        lexer->result_symbol = VARSUB_PREFIX;
        return true;
    }

    if (is_concat_valid(lexer, valid_symbols)) {
        myprintf("We picked concat\n");
        lexer->result_symbol = CONCAT;
        return true;
    }

    myprintf("We picked nothing\n");
    return false;
}

unsigned tree_sitter_tcl_external_scanner_serialize(void *payload, char *buffer) {
    Scanner *scanner = (Scanner *)payload;

    size_t size = 0;
    buffer[size++] = (char)scanner->expr_depth;
    buffer[size++] = (char)scanner->cmdsub_depth;

    return size;
}

void tree_sitter_tcl_external_scanner_deserialize(void *payload, const char *buffer, unsigned length){
    Scanner *scanner = (Scanner *)payload;

    size_t size = 0;

    if (length > 0) {
        scanner->expr_depth = (int8_t) buffer[size++];
        scanner->cmdsub_depth = (int8_t) buffer[size++];
    }
}

void tree_sitter_tcl_external_scanner_destroy(void *payload) {
    Scanner *scanner = (Scanner *)payload;
    free(scanner);
}

void *tree_sitter_tcl_external_scanner_create() {
    Scanner *scanner = calloc(1, sizeof(Scanner));
    tree_sitter_tcl_external_scanner_deserialize(scanner, NULL, 0);
    return scanner;
}


/* vim: set ts=4 sw=4 et : */
