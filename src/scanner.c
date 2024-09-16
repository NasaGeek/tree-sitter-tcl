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

static bool is_simple_word(TSLexer *lexer, Scanner *scanner) {
    // Trying to match simple_word in grammar.js as closely as possible
    int32_t chr = lexer->lookahead;
    return (!iswspace(chr) &&
            !lexer->eof(lexer) &&
            chr != '\\' &&
            chr != '[' &&
            (chr != ']' || scanner->cmdsub_depth == 0) && // can only concat ] outside of cmdsub
            chr != '{' &&
            chr != '}' &&
            // need to continue excluding ( and ) so we recognize the starts
            // of array indexing
            chr != '(' &&
            chr != ')' &&
            chr != ';');
}

static bool is_tcl_whitespace(int32_t chr) {
    return iswspace(chr) && chr != '\n';
}

static bool is_tcl_separator(int32_t chr) {
    return chr == '\n' || chr == ';';
}

static bool is_concat_valid(TSLexer *lexer, const bool *valid_symbols, Scanner *scanner) {
    /* Don't yet have a good answer to how we allow stuff like ]/{/}/" in strings
     * that didn't start with " or {
     */
    if (valid_symbols[CONCAT]) {
        if (is_simple_word(lexer, scanner) ||
                lexer->lookahead == '$' ||
                lexer->lookahead == '[') {
            return true;
        }
    }
    return false;
}

static bool has_followup_command(TSLexer *lexer) {
    return !(lexer->lookahead == ']' || lexer->lookahead == '}' || lexer->eof(lexer));
}

enum colon_type { NAMESPACE, PLAIN, NONE };

static enum colon_type scan_ns_and_plain_colon(TSLexer *lexer) {
    if (lexer->lookahead == ':') {
        lexer->advance(lexer, false);
        if (lexer->lookahead == ':') {
            lexer->advance(lexer, false);
            while (lexer->lookahead == ':') {
                // https://www.tcl-lang.org/man/tcl8.6/TclCmd/namespace.htm#:~:text=two%20or%20more%20colons%20are%20treated%20as%20a%20namespace%20separator
                lexer->advance(lexer, false);
            }
            return NAMESPACE;
        } else {
            return PLAIN;
        }
    } else {
        return NONE;
    }
}

static bool scan_variable_substitution(TSLexer *lexer) {
    // Why do I have to eat whitespace for this but not other stuff? What's
    // special about this token?
    // Beware the \n check, might break stuff outside of exprs
    while (true) {
        if (is_tcl_whitespace(lexer->lookahead) || lexer->lookahead == '\n') {
            lexer->advance(lexer, true);
            continue;
        }
        lexer->mark_end(lexer);
        if (lexer->lookahead == '\\') {
            lexer->advance(lexer, true);
            if (lexer->lookahead == '\n') {
                lexer->advance(lexer, true);
                continue;
            } else {
                return false;
            }
        }
        break;
    }
    if (lexer->lookahead == '$') {
        lexer->mark_end(lexer);
        lexer->advance(lexer, false);
        return lexer->lookahead == '{' ||
            // Empty array name
            lexer->lookahead == '(' ||
            is_bare_word(lexer->lookahead) ||
            // Advances lexer, must go last
            scan_ns_and_plain_colon(lexer) == NAMESPACE;
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
    }
    return saw_gap;
}

static bool is_command_end(TSLexer *lexer, Scanner *scanner) {
    int32_t chr = lexer->lookahead;
    // TODO: quotes should go here, too, but they'll need special tracking to
    // identify closing vs opening quotes
    return (chr == ';' ||
            chr == '\n' ||
            (chr == ']' && scanner->cmdsub_depth > 0) ||
            chr == '}');
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
        myprintf("cmdsub_end\n");
        return true;
    }

    if (valid_symbols[EXPR_END]) {
        scanner->expr_depth--;
        lexer->result_symbol = EXPR_END;
        myprintf("expr_end\n");
        return true;
    }


    if (lexer->lookahead == '\n' && !valid_symbols[NEWLINE] && scanner->expr_depth == 0) {
        // This seems extremely brittle...
        // Trying to avoid treating newlines as extras except when we're inside a
        // braced expr.
        // Placement is tricky, needs to go after any zero-length matches possibly.
        // Consider merging with the very similar handling of NEWLINE below.
        // Specifically added this for proper rejection of unbraced exprs like
        // expr 1+
        // 1
        lexer->result_symbol = NEWLINE;
        myprintf("Consuming newline so it's not extrafied\n");
        return true;
    }

    if (lexer->lookahead == '\\') {
        // consumes input, where to put it?
        lexer->mark_end(lexer);
        lexer->advance(lexer, false);
        if (valid_symbols[GAP] && lexer->lookahead == '\n') {
            // Probably need better checking that includes \r? along with several other spots...
            lexer->advance(lexer, false);
            lexer->mark_end(lexer);
            lexer->result_symbol = GAP;
            myprintf("\\ gap\n");
            return true;
        } else if (valid_symbols[VARSUB_PREFIX] && lexer->lookahead == '\n') {
            lexer->advance(lexer, true);
            int32_t saved_col = lexer->get_column(lexer);
            if (scan_variable_substitution(lexer)) {
                // So weird that I have to check for this. It seems like it's
                // allowing extras between _varsub_prefix and the $ even though
                // I've marked the $ as token.immediate()
                myprintf("\\ varsub\n");
                lexer->result_symbol = VARSUB_PREFIX;
                return true;
            } else {
                if (lexer->get_column(lexer) != saved_col) {
                    // We advanced, rest of the lex is spoiled
                    return false;
                }
            }
        } else if (valid_symbols[CONCAT]) {
            // could we be safe making this just an `else`? Would simplify
            // assumptions below
            myprintf("\\ concat\n");
            lexer->result_symbol = CONCAT;
            return true;
        } else {
            // I don't think anything else should be able to parse a \, but
            // maybe this'll have to change. As it stands, without this, we
            // could end up treating \$a like a varsub.
            myprintf("\\ but no match, looking at '%c'\n", lexer->lookahead);
            return false;
        }
    }

    bool gap_ate_whitespace = false;
    if (valid_symbols[GAP]) {
        if (scanner->cmdsub_depth >= scanner->expr_depth && scan_gap(lexer)) {
            // This might end up too fragile and it'd be better to just
            // allow optional gap after args
            if (!is_command_end(lexer, scanner)) {
                lexer->result_symbol = GAP;
                myprintf("we picked gap\n");
                return true;
            } else {
                // Lexer was advanced, so we gotta bail
                gap_ate_whitespace = true;
            }
        }
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
        // I'm split on whether we return false here. We generally want to give
        // other rules a chance to match, but this has also potentially consumed
        // input which could throw things off? I'll settle for careful ordering
        // for now.
        return false;
    }

    if (valid_symbols[NEWLINE] && scanner->expr_depth == 0 && lexer->lookahead == '\n') {
        // Ensure this is below command separator so it doesn't steal the newline from it
        // Treat it as an extra only inside exprs
        lexer->advance(lexer, false);
        lexer->result_symbol = NEWLINE;
        myprintf("newline\n");
        return true;
    }
    // Trying to stop the advanced lexer from GAP from incluencing any
    // non-whitspace rules that proceed it, but this feels hairy
    if (gap_ate_whitespace) {
        return false;
    }


    if (valid_symbols[NS_DELIM]) {
        lexer->mark_end(lexer);
        enum colon_type c = scan_ns_and_plain_colon(lexer);
        if (c == NAMESPACE) {
            myprintf("We picked ns\n");
            lexer->mark_end(lexer);
            lexer->result_symbol = NS_DELIM;
            return true;
        } else if (valid_symbols[CONCAT] && c == PLAIN) {
            // NB: CONCAT should always be set if NS_DELIM is set I think
            myprintf("We picked concat, %c\n", lexer->lookahead);
            lexer->result_symbol = CONCAT;
            return true;
        }
    }

    if (valid_symbols[VARSUB_PREFIX]) {
        int32_t saved_col = lexer->get_column(lexer);
        if (scan_variable_substitution(lexer)) {
            myprintf("We picked varsub\n");
            lexer->result_symbol = VARSUB_PREFIX;
            return true;
        } else if (lexer->get_column(lexer) != saved_col) {
            // We advanced, rest of the lex is spoiled
            myprintf("Never saw varsub but advanced lexer, bailing\n");
            return false;
        }
    }

    if (is_concat_valid(lexer, valid_symbols, scanner)) {
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
