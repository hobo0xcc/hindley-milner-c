#include "hm.h"

void test_parse1() {
    char *src = "a b c 12 34 56\n";
    Vector *tokens = tokenize(src);
    PEnv *e = new_parser_env(tokens);

    assert(vector_size(tokens) == 6);
    assert(advance(e)->ty == TK_IDENT);
    assert(advance(e)->ty == TK_IDENT);
    assert(advance(e)->ty == TK_IDENT);
    assert(advance(e)->ty == TK_INT);
    assert(advance(e)->ty == TK_INT);
    assert(advance(e)->ty == TK_INT);
}

void test_parse2() {
    char *src = "2 + 3 * 4";
    Vector *tokens = tokenize(src);
    Node *result = parse(tokens);
    print_node(result);
}

void test_parse3() {
    char *src = "fun x() = 2 + x";
    Vector *tokens = tokenize(src);
    Node *result = parse(tokens);
    print_node(result);
}

void test_parse4() {
    char *src = "if a then 2 else 42";
    Vector *tokens = tokenize(src);
    Node *result = parse(tokens);
    print_node(result);
}

void test_parse5() {
    char *src = "3 + 4 * 3 + 3 + 4 * 5 / 2 - 2 * 3";
    Vector *tokens = tokenize(src);
    Node *result = parse(tokens);
    print_node(result);
}
