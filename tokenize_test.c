#include "hm.h"

void test_tokenize1() {
    char *test1 = "Hello world 1 2 33 44 55";
    Vector *tokens = tokenize(test1);
    int ty[] = {
        TK_IDENT,
        TK_IDENT,
        TK_INT,
        TK_INT,
        TK_INT,
        TK_INT,
        TK_INT,
        0,
    };

    for (int i = 0; i < vector_size(tokens); i++) {
        Token *tk = vector_at(tokens, i);
        if (ty[i] == 0) error("out of index");
        assert(tk->ty == ty[i]);
    }
}
