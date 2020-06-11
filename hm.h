#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

void error(const char *fmt, ...);

typedef struct Vector Vector;
struct Vector {
    int capacity;
    int size;
    void **data;
};

typedef struct Map Map;
struct Map {
    Vector *keys;
    Vector *items;
    int len;
};

// Vector functions
Vector *new_vector();
bool vector_grow(Vector *vec, int size);
bool vector_push_back(Vector *vec, void *item);
bool vector_set(Vector *vec, int index, void *item);
void *vector_at(Vector *vec, int index);
void vector_alloc(Vector *vec, int size);
int vector_size(Vector *vec);
// Map functions
Map *new_map();
void map_set(Map *mp, char *key, void *item);
void *map_get(Map *mp, char *key);
int map_len(Map *mp);

// Tokenizer
enum TokenType {
    TK_IDENT = 256,
    TK_INT,
    TK_TRUE,
    TK_FALSE,
    TK_IF,
    TK_THEN,
    TK_ELSE,
    TK_FUN,
    TK_PLUS,
    TK_MINUS,
    TK_ASTERISK,
    TK_SLASH,
    TK_EQUAL,
    TK_EQQ,
    TK_NEQ,
    TK_LPAREN,
    TK_RPAREN,
    TK_COMMA,
    TK_ILLEGAL,
    TK_EOF,
};

typedef struct Token Token;
struct Token {
    int ty;
    char *str;
};

Token *new_token(int ty, char *str);
char *substr(char *start, char *end);
Token *next(char **p);
Vector *tokenize(char *src);
char *type_to_string(int ty);

// Parser
typedef struct PEnv PEnv;
struct PEnv {
    Vector *tokens;
    int cur;
};

enum NodeType {
    ND_NUM = 256,
    ND_VAL,
    ND_BIN,
    ND_IF,
    ND_APP,
    ND_FUN,
    ND_TOPLEVEL,
};

enum Op {
    OP_ADD = 256,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_EQUAL,
    OP_NOT_EQUAL,
};

enum TypeKind {
    T_UNKNOWN,
    T_INT,
    T_BOOL,
    T_VAR,
    T_APP,
    T_FUN,
};

typedef struct ASTTopLevel ASTTopLevel;
typedef struct ASTNumExpr ASTNumExpr;
typedef struct ASTValExpr ASTValExpr;
typedef struct ASTBinExpr ASTBinExpr;
typedef struct ASTIfExpr ASTIfExpr;
typedef struct ASTAppExpr ASTAppExpr;
typedef struct ASTFunExpr ASTFunExpr;
typedef struct Type Type;
typedef struct Node Node;

struct ASTTopLevel {
    Vector *exprs;
};

struct ASTNumExpr {
    int num;
};

struct ASTValExpr {
    char *ident;
};

struct ASTBinExpr {
    Node *lhs;
    int op;
    Node *rhs;
};

struct ASTIfExpr {
    Node *cond;
    Node *thenbody;
    Node *elsebody;
};

struct ASTAppExpr {
    char *name;
    Vector *args;
};

struct ASTFunExpr {
    char *name;
    Vector *args;
    Node *body;
    Map *arg_types;
};

struct Type {
    int kind;
    int tvar;
    Type *ret_type;
    Vector *arg_types;
};

struct Node {
    int kind;
    Type *type;
    union {
        ASTTopLevel *toplevel;
        ASTNumExpr *numexpr;
        ASTValExpr *valexpr;
        ASTBinExpr *binexpr;
        ASTIfExpr *ifexpr;
        ASTAppExpr *appexpr;
        ASTFunExpr *funexpr;
    } ast;
};

PEnv *new_parser_env(Vector *tokens);
Token *curr(PEnv *e);
Token *peek(PEnv *e, int offset);
Token *advance(PEnv *e);
bool match(PEnv *e, int ty);
bool peek_match(PEnv *e, int offset, int ty);
Token *expect(PEnv *e, int ty);

Type *new_type();
Node *new_node(int ty, void *ast);
ASTNumExpr *new_numexpr(int num);
ASTValExpr *new_valexpr(char *ident);
ASTBinExpr *new_binexpr(Node *lhs, int op, Node *rhs);
ASTIfExpr *new_ifexpr(Node *cond, Node *thenbody, Node *elsebody);
ASTAppExpr *new_appexpr(char *name, Vector *args);
ASTFunExpr *new_funexpr(char *name, Vector *args, Node *body);
ASTTopLevel *new_toplevel(Vector *exprs);

char *op_to_string(int op);
char *node_type_to_string(int ty);
void print_node(Node *nd);

Node *ifexpr(PEnv *e);
Node *funexpr(PEnv *e);
Node *primary_expr(PEnv *e);
Node *mul_expr(PEnv *e);
Node *add_expr(PEnv *e);
Node *expr(PEnv *e);

Node *parse(Vector *tokens);

// Typing
typedef struct TEnv TEnv;
typedef struct TypeEquation TypeEquation;

struct TEnv {
    int tvar;
    Type *inttype;
    Type *booltype;
    Map *symtab;
};
struct TypeEquation {
    Type *lhs;
    Type *rhs;
    Node *orig;
};

TEnv *new_typing_env();
TypeEquation *new_type_eq(Type *lhs, Type *rhs, Node *orig);

void annotate_node(TEnv *e, Node *nd);
void gen_equation(TEnv *e, Node *nd, Vector *equations);
bool unify(Type *lhs, Type *rhs, Vector *subst);
bool unify_variable(Type *v, Type *x, Vector *subst);
bool occurs_check(Type *v, Type *x, Vector *subst);
Vector *unify_all_equations(TEnv *e, Vector *equations);
char *tvar_to_string(int tvar_num);
void print_type(Type *t);
void print_type_equations(Vector *equations);
void print_all_node_type(Node *nd);
void print_subst(Vector *subst);
Type *get_real_type(Type *t, Vector *subst);
void print_typed_node(Node *nd, Vector *subst);

// Test
void test_vector();
void test_map();
void test_tokenize1();
void test_parse1();
void test_parse2();
void test_parse3();
void test_parse4();
void test_parse5();
