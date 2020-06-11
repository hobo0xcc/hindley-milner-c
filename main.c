#include "hm.h"

void test() {
    test_vector();
    test_map();
    test_tokenize1();
    test_parse1();
    test_parse2();
    test_parse3();
    test_parse4();
    test_parse5();
}

int main(int argc, char **argv) {
    if (argc < 2) {
        error("missing argument");
    }

    if (strcmp(argv[1], "--test") == 0) {
        test();
        printf("Test succeeded\n");
        return 0;
    }

    FILE *fp;
    if ((fp = fopen(argv[1], "rb")) == NULL) {
        error("at open the file");
    }

    fseek(fp, 0, SEEK_END);
    int length = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buf = malloc(length);
    fread(buf, length, 1, fp);

    Vector *tokens = tokenize(buf);
    Node *nd = parse(tokens);
    TEnv *te = new_typing_env();
    annotate_node(te, nd);
    Vector *equations = new_vector();
    gen_equation(te, nd, equations);
    Vector *subst = unify_all_equations(te, equations);
    if (subst == NULL) {
        error("unify failed");
    }
    printf("%s\n----------\n", buf);
    print_typed_node(nd, subst);
    return 0;
}
