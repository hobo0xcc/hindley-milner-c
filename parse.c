#include "hm.h"

PEnv *new_parser_env(Vector *tokens) {
    PEnv *e = malloc(sizeof(PEnv));
    e->tokens = tokens;
    e->cur = 0;
    return e;
}

Token *curr(PEnv *e) {
    int size = vector_size(e->tokens);
    if (size <= e->cur) {
        Token *tk = new_token(TK_EOF, NULL);
        return tk;
    }
    return vector_at(e->tokens, e->cur);
}

Token *peek(PEnv *e, int offset) {
    int size = vector_size(e->tokens);
    if (size <= e->cur + offset) {
        Token *eof = new_token(TK_EOF, NULL);
        return eof;
    }

    return vector_at(e->tokens, e->cur + offset);
}

Token *advance(PEnv *e) {
    int size = vector_size(e->tokens);
    if (size <= e->cur) {
        Token *eof = new_token(TK_EOF, NULL);
        return eof;
    }

    return vector_at(e->tokens, e->cur++);
}

bool match(PEnv *e, int ty) {
    if (curr(e)->ty == ty)
        return true;
    return false;
}

bool peek_match(PEnv *e, int offset, int ty) {
    if (peek(e, offset)->ty == ty)
        return true;
    return false;
}

Token *expect(PEnv *e, int ty) {
    Token *tk = advance(e);
    if (tk->ty == ty)
        return tk;
    error("expected %s but got %s", type_to_string(ty), type_to_string(tk->ty));
    return NULL;
}

Type *new_type() {
    Type *ty = malloc(sizeof(Type));
    ty->kind = T_UNKNOWN;
    return ty;
}

Node *new_node(int ty, void *ast) {
    Node *nd = malloc(sizeof(Node));
    nd->kind = ty;
    nd->type = new_type();
    switch (ty) {
    case ND_NUM:
        nd->ast.numexpr = ast;
        break;
    case ND_VAL:
        nd->ast.valexpr = ast;
        break;
    case ND_BIN:
        nd->ast.binexpr = ast;
        break;
    case ND_IF:
        nd->ast.ifexpr = ast;
        break;
    case ND_APP:
        nd->ast.appexpr = ast;
        break;
    case ND_FUN:
        nd->ast.funexpr = ast;
        break;
    case ND_TOPLEVEL:
        nd->ast.toplevel = ast;
        break;
    default:
        break;
    }

    return nd;
}

ASTNumExpr *new_numexpr(int num) {
    ASTNumExpr *a = malloc(sizeof(ASTNumExpr));
    a->num = num;
    return a;
}

ASTValExpr *new_valexpr(char *ident) {
    ASTValExpr *a = malloc(sizeof(ASTValExpr));
    a->ident = ident;
    return a;
}

ASTBinExpr *new_binexpr(Node *lhs, int op, Node *rhs) {
    ASTBinExpr *a = malloc(sizeof(ASTBinExpr));
    a->lhs = lhs;
    a->op = op;
    a->rhs = rhs;
    return a;
}

ASTIfExpr *new_ifexpr(Node *cond, Node *thenbody, Node *elsebody) {
    ASTIfExpr *a = malloc(sizeof(ASTIfExpr));
    a->cond = cond;
    a->thenbody = thenbody;
    a->elsebody = elsebody;
    return a;
}

ASTAppExpr *new_appexpr(char *name, Vector *args) {
    ASTAppExpr *a = malloc(sizeof(ASTAppExpr));
    a->name = name;
    a->args = args;
    return a;
}

ASTFunExpr *new_funexpr(char *name, Vector *args, Node *body) {
    ASTFunExpr *a = malloc(sizeof(ASTFunExpr));
    a->name = name;
    a->args = args;
    a->body = body;
    return a;
}

ASTTopLevel *new_toplevel(Vector *exprs) {
    ASTTopLevel *a = malloc(sizeof(ASTTopLevel));
    a->exprs = exprs;
    return a;
}

char *op_to_string(int op) {
    switch (op) {
    case OP_ADD:
        return "+";
    case OP_SUB:
        return "-";
    case OP_MUL:
        return "*";
    case OP_DIV:
        return "/";
    case OP_EQUAL:
        return "==";
    case OP_NOT_EQUAL:
        return "!=";
    default:
        return NULL;
    }
}

char *node_type_to_string(int ty) {
    switch (ty) {
    case ND_NUM:
        return "NUM";
    case ND_VAL:
        return "VAL";
    case ND_BIN:
        return "BIN";
    case ND_IF:
        return "IF";
    case ND_FUN:
        return "FUN";
    case ND_TOPLEVEL:
        return "TOPLEVEL";
    default:
        return NULL;
    }
}

// For AST Debug
void print_node(Node *nd) {
    if (nd->kind == ND_NUM) {
        printf("%d", nd->ast.numexpr->num);
    } else if (nd->kind == ND_VAL) {
        printf("%s", nd->ast.valexpr->ident);
    } else if (nd->kind == ND_BIN) {
        printf("(");
        print_node(nd->ast.binexpr->lhs);
        printf("%s", op_to_string(nd->ast.binexpr->op));
        print_node(nd->ast.binexpr->rhs);
        printf(")");
    } else if (nd->kind == ND_IF) {
        printf("(if ");
        print_node(nd->ast.ifexpr->cond);
        printf(" then ");
        print_node(nd->ast.ifexpr->thenbody);
        printf(" else ");
        print_node(nd->ast.ifexpr->elsebody);
        printf(")");
    } else if (nd->kind == ND_APP) {
        printf("(app ");
        printf("%s ", nd->ast.appexpr->name);
        int size = vector_size(nd->ast.appexpr->args);
        printf("(");
        for (int i = 0; i < size; i++) {
            Node *arg = vector_at(nd->ast.appexpr->args, i);
            print_node(arg);
            if (i != size - 1)
                printf(", ");
        }
        printf(")");
        printf(")");
    } else if (nd->kind == ND_FUN) {
        printf("(fun ");
        printf("%s ", nd->ast.funexpr->name);
        int size = vector_size(nd->ast.funexpr->args);
        for (int i = 0; i < size; i++) {
            char *name = vector_at(nd->ast.funexpr->args, i);
            printf("%s", name);
            if (i != size - 1)
                printf(" ");
        }
        printf(" ");
        print_node(nd->ast.funexpr->body);
        printf(")");
    } else if (nd->kind == ND_TOPLEVEL) {
        for (int i = 0; i < vector_size(nd->ast.toplevel->exprs); i++) {
            Node *item = vector_at(nd->ast.toplevel->exprs, i);
            print_node(item);
            printf("\n");
        }
    } else {
        return;
    }
}

Node *ifexpr(PEnv *e) {
    expect(e, TK_IF);
    Node *cond = expr(e);
    expect(e, TK_THEN);
    Node *thenbody = expr(e);
    expect(e, TK_ELSE);
    Node *elsebody = expr(e);

    ASTIfExpr *a = new_ifexpr(cond, thenbody, elsebody);
    Node *nd = new_node(ND_IF, a);
    return nd;
}

Node *funexpr(PEnv *e) {
    expect(e, TK_FUN);
    Vector *args = new_vector();
    Token *name = expect(e, TK_IDENT);
    expect(e, TK_LPAREN);
    while (!match(e, TK_RPAREN)) {
        Token *arg = expect(e, TK_IDENT);
        vector_push_back(args, arg->str);
        if (!match(e, TK_RPAREN)) {
            expect(e, TK_COMMA);
        }
    }
    expect(e, TK_RPAREN);
    expect(e, TK_EQUAL);
    Node *body = expr(e);
    ASTFunExpr *funexp = new_funexpr(name->str, args, body);
    Node *fun = new_node(ND_FUN, funexp);
    return fun;
}

Node *primary_expr(PEnv *e) {
    if (match(e, TK_LPAREN)) {
        expect(e, TK_LPAREN);
        Node *exp = expr(e);
        expect(e, TK_RPAREN);
        return exp;
    } else if (match(e, TK_INT)) {
        Token *tk = advance(e);
        ASTNumExpr *numexp = new_numexpr(atoi(tk->str));
        Node *num = new_node(ND_NUM, numexp);
        return num;
    } else if (match(e, TK_IDENT)) {
        if (peek_match(e, 1, TK_LPAREN)) {
            Token *tk = advance(e);
            expect(e, TK_LPAREN);
            Vector *args = new_vector();
            while (!match(e, TK_RPAREN)) {
                Node *arg = expr(e);
                vector_push_back(args, arg);
                if (!match(e, TK_RPAREN)) {
                    expect(e, TK_COMMA);
                }
            }
            expect(e, TK_RPAREN);
            ASTAppExpr *app = new_appexpr(tk->str, args);
            Node *appexp = new_node(ND_APP, app);
            return appexp;
        } else {
            Token *tk = advance(e);
            ASTValExpr *valexp = new_valexpr(tk->str);
            Node *val = new_node(ND_VAL, valexp);
            return val;
        }
    } else {
        Token *tk = curr(e);
        error("unknown Token: %s", type_to_string(tk->ty));
        return NULL;
    }
}

Node *mul_expr(PEnv *e) {
    Node *lhs = primary_expr(e);
    for (;;) {
        if (match(e, TK_ASTERISK) || match(e, TK_SLASH)) {
            int op;
            if (match(e, TK_ASTERISK))
                op = OP_MUL;
            else
                op = OP_DIV;

            advance(e);
            Node *rhs = primary_expr(e);
            ASTBinExpr *bin = new_binexpr(lhs, op, rhs);
            lhs = new_node(ND_BIN, bin);
        } else {
            break;
        }
    }

    return lhs;
}

Node *add_expr(PEnv *e) {
    Node *lhs = mul_expr(e);
    for (;;) {
        if (match(e, TK_PLUS) || match(e, TK_MINUS)) {
            int op;
            if (match(e, TK_PLUS))
                op = OP_ADD;
            else
                op = OP_SUB;

            advance(e);
            Node *rhs = mul_expr(e);
            ASTBinExpr *bin = new_binexpr(lhs, op, rhs);
            lhs = new_node(ND_BIN, bin);
        } else {
            break;
        }
    }

    return lhs;
}

Node *equal_expr(PEnv *e) {
    Node *lhs = add_expr(e);
    for (;;) {
        if (match(e, TK_EQQ) || match(e, TK_NEQ)) {
            int op;
            if (match(e, TK_EQQ))
                op = OP_EQUAL;
            else
                op = OP_NOT_EQUAL;

            advance(e);
            Node *rhs = add_expr(e);
            ASTBinExpr *bin = new_binexpr(lhs, op, rhs);
            lhs = new_node(ND_BIN, bin);
        } else {
            break;
        }
    }

    return lhs;
}

Node *expr(PEnv *e) {
    if (match(e, TK_IF)) {
        return ifexpr(e);
    } else if (match(e, TK_FUN)) {
        return funexpr(e);
    } else {
        return equal_expr(e);
    }
}

Node *parse(Vector *tokens) {
    PEnv *e = new_parser_env(tokens);
    Vector *exprs = new_vector();
    while (!match(e, TK_EOF)) {
        vector_push_back(exprs, expr(e));
    }

    ASTTopLevel *toplevel = new_toplevel(exprs);
    Node *nd = new_node(ND_TOPLEVEL, toplevel);
    return nd;
}
