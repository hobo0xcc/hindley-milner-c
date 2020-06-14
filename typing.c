#include "hm.h"

TEnv *new_typing_env() {
    TEnv *e = malloc(sizeof(TEnv));
    e->tvar = 0;
    Type *inttype = new_type();
    Type *booltype = new_type();
    inttype->kind = T_INT;
    booltype->kind = T_BOOL;
    e->inttype = inttype;
    e->booltype = booltype;
    e->symtab = new_map();
    return e;
}

TypeEquation *new_type_eq(Type *lhs, Type *rhs, Node *orig) {
    TypeEquation *eq = malloc(sizeof(TypeEquation));
    eq->lhs = lhs;
    eq->rhs = rhs;
    eq->orig = orig;
    return eq;
}

/* Set initial types to ast node `nd`.
 * For instance, if node has integer the node is typed with type T_INT.
 * If node's type is currently undecidable,
 * it is typed with T_VAR, which means that the type has a Type variable.
 * */
void annotate_node(TEnv *e, Node *nd) {
    if (nd->kind == ND_NUM) {
        nd->type->kind = T_INT;
    } else if (nd->kind == ND_VAL) {
        Type *type_kind = map_get(e->symtab, nd->ast.valexpr->ident);
        if (type_kind == NULL) {
            error("unbound identifier %s", nd->ast.valexpr->ident);
        }

        nd->type = type_kind;
    } else if (nd->kind == ND_BIN) {
        annotate_node(e, nd->ast.binexpr->lhs);
        annotate_node(e, nd->ast.binexpr->rhs);
        Type *lhs_type = nd->ast.binexpr->lhs->type;
        Type *rhs_type = nd->ast.binexpr->rhs->type;
        Type *expr_type = new_type();
        int op = nd->ast.binexpr->op;
        if (op == OP_ADD || op == OP_SUB || op == OP_MUL || op == OP_DIV) {
            expr_type->kind = T_INT;
        } else if (op == OP_EQUAL || op == OP_NOT_EQUAL) {
            expr_type->kind = T_BOOL;
        } else {
            error("unknown operator: %s", op_to_string(op));
        }

        nd->type = expr_type;
    } else if (nd->kind == ND_IF) {
        annotate_node(e, nd->ast.ifexpr->cond);
        annotate_node(e, nd->ast.ifexpr->thenbody);
        annotate_node(e, nd->ast.ifexpr->elsebody);
        nd->type = nd->ast.ifexpr->thenbody->type;
    } else if (nd->kind == ND_FUN) {
        int size = vector_size(nd->ast.funexpr->args);
        Map *arg_types = new_map();
        for (int i = 0; i < size; i++) {
            Type *arg_type = new_type();
            arg_type->kind = T_VAR;
            arg_type->tvar = e->tvar++;
            char *name = vector_at(nd->ast.funexpr->args, i);
            map_set(arg_types, name, arg_type);
            map_set(e->symtab, name, arg_type);
        }

        Type *fun_type = new_type();
        fun_type->kind = T_VAR;
        fun_type->tvar = e->tvar++;
        nd->type = fun_type;
        nd->ast.funexpr->arg_types = arg_types;
        map_set(e->symtab, nd->ast.funexpr->name, fun_type);

        annotate_node(e, nd->ast.funexpr->body);
    } else if (nd->kind == ND_APP) {
        int size = vector_size(nd->ast.appexpr->args);
        for (int i = 0; i < size; i++) {
            Node *arg = vector_at(nd->ast.appexpr->args, i);
            annotate_node(e, arg);
        }

        Type *apptype =
            new_type(); // map_get(e->symtab, nd->ast.appexpr->name);
        apptype->kind = T_VAR;
        apptype->tvar = e->tvar++;
        if (apptype == NULL) {
            error("undeclared function");
        }
        nd->type = apptype;
    } else if (nd->kind == ND_TOPLEVEL) {
        int size = vector_size(nd->ast.toplevel->exprs);
        for (int i = 0; i < size; i++) {
            Node *expr = vector_at(nd->ast.toplevel->exprs, i);
            annotate_node(e, expr);
        }
    }
}

/* Generate Type equations.
 * Type equations are used to inference types by unifying those equations.
 * */
void gen_equation(TEnv *e, Node *nd, Vector *equations) {
    if (nd->kind == ND_NUM) {
        TypeEquation *eq = new_type_eq(nd->type, e->inttype, nd);
        vector_push_back(equations, eq);
    } else if (nd->kind == ND_BIN) {
        gen_equation(e, nd->ast.binexpr->lhs, equations);
        gen_equation(e, nd->ast.binexpr->rhs, equations);
        int op = nd->ast.binexpr->op;
        if (op == OP_ADD || op == OP_SUB || op == OP_MUL || op == OP_DIV) {
            TypeEquation *eq = new_type_eq(nd->type, e->inttype, nd);
            TypeEquation *lhs_eq = new_type_eq(
                nd->ast.binexpr->lhs->type, e->inttype, nd->ast.binexpr->lhs);
            TypeEquation *rhs_eq = new_type_eq(
                nd->ast.binexpr->rhs->type, e->inttype, nd->ast.binexpr->rhs);
            vector_push_back(equations, eq);
            vector_push_back(equations, lhs_eq);
            vector_push_back(equations, rhs_eq);
        } else if (op == OP_EQUAL || op == OP_NOT_EQUAL) {
            TypeEquation *eq = new_type_eq(nd->type, e->booltype, nd);
            TypeEquation *lhs_eq = new_type_eq(
                nd->ast.binexpr->lhs->type, e->inttype, nd->ast.binexpr->lhs);
            TypeEquation *rhs_eq = new_type_eq(
                nd->ast.binexpr->rhs->type, e->inttype, nd->ast.binexpr->rhs);
            vector_push_back(equations, eq);
            vector_push_back(equations, lhs_eq);
            vector_push_back(equations, rhs_eq);
        } else {
            error("unknown operator");
        }

        TypeEquation *lhs_rhs_eq = new_type_eq(nd->ast.binexpr->lhs->type,
                                               nd->ast.binexpr->rhs->type, nd);
        vector_push_back(equations, lhs_rhs_eq);
    } else if (nd->kind == ND_IF) {
        gen_equation(e, nd->ast.ifexpr->cond, equations);
        gen_equation(e, nd->ast.ifexpr->thenbody, equations);
        gen_equation(e, nd->ast.ifexpr->elsebody, equations);

        TypeEquation *condeq = new_type_eq(nd->ast.ifexpr->cond->type,
                                           e->booltype, nd->ast.ifexpr->cond);
        TypeEquation *theneq = new_type_eq(nd->ast.ifexpr->thenbody->type,
                                           nd->type, nd->ast.ifexpr->thenbody);
        TypeEquation *elseeq = new_type_eq(nd->ast.ifexpr->elsebody->type,
                                           nd->type, nd->ast.ifexpr->elsebody);
        TypeEquation *then_else_eq = new_type_eq(
            nd->ast.ifexpr->thenbody->type, nd->ast.ifexpr->elsebody->type, nd);

        vector_push_back(equations, condeq);
        vector_push_back(equations, then_else_eq);
        vector_push_back(equations, theneq);
        vector_push_back(equations, elseeq);
    } else if (nd->kind == ND_FUN) {
        Vector *arg_types = new_vector();
        int size = vector_size(nd->ast.funexpr->args);
        for (int i = 0; i < size; i++) {
            char *name = vector_at(nd->ast.funexpr->args, i);
            vector_push_back(arg_types,
                             map_get(nd->ast.funexpr->arg_types, name));
        }
        Type *ty = new_type();
        ty->kind = T_FUN;
        ty->ret_type = nd->ast.funexpr->body->type;
        ty->arg_types = arg_types;
        TypeEquation *fun_eq = new_type_eq(nd->type, ty, nd);
        vector_push_back(equations, fun_eq);

        gen_equation(e, nd->ast.funexpr->body, equations);
    } else if (nd->kind == ND_APP) {
        int size = vector_size(nd->ast.appexpr->args);
        for (int i = 0; i < size; i++) {
            Node *arg = vector_at(nd->ast.appexpr->args, i);
            gen_equation(e, arg, equations);
        }

        Vector *arg_types = new_vector();
        for (int i = 0; i < size; i++) {
            Node *arg = vector_at(nd->ast.appexpr->args, i);
            vector_push_back(arg_types, arg->type);
        }

        Type *app_fun_type = map_get(e->symtab, nd->ast.appexpr->name);
        Type *fun_type = new_type();
        fun_type->kind = T_FUN;
        fun_type->arg_types = arg_types;
        fun_type->ret_type = nd->type;
        TypeEquation *eq = new_type_eq(app_fun_type, fun_type, nd);
        vector_push_back(equations, eq);
    } else if (nd->kind == ND_TOPLEVEL) {
        int size = vector_size(nd->ast.toplevel->exprs);
        for (int i = 0; i < size; i++) {
            Node *expr = vector_at(nd->ast.toplevel->exprs, i);
            gen_equation(e, expr, equations);
        }
    }

    if (nd->type == T_UNKNOWN) {
        printf("%s", node_type_to_string(nd->kind));
    }
}

/*
 * Unify equations to generate substitutions of type variables.
 * */
bool unify(Type *lhs, Type *rhs, Vector *subst) {
    if (lhs->kind != T_VAR && lhs->kind != T_FUN && lhs->kind == rhs->kind) {
        return true;
    } else if (lhs->kind == T_VAR && rhs->kind == T_VAR &&
               lhs->tvar == rhs->tvar) {
        return true;
    } else if (lhs->kind == T_VAR) {
        return unify_variable(lhs, rhs, subst);
    } else if (rhs->kind == T_VAR) {
        return unify_variable(rhs, lhs, subst);
    } else if (lhs->kind == T_FUN && rhs->kind == T_FUN) {
        if (vector_size(lhs->arg_types) != vector_size(rhs->arg_types)) {
            return false;
        } else {
            bool ok = unify(lhs->ret_type, rhs->ret_type, subst);
            if (!ok)
                return false;
            int size = vector_size(lhs->arg_types);
            for (int i = 0; i < size; i++) {
                Type *larg_ty = vector_at(lhs->arg_types, i);
                Type *rarg_ty = vector_at(rhs->arg_types, i);
                bool ok = unify(larg_ty, rarg_ty, subst);
                if (!ok)
                    return false;
            }

            return true;
        }
    }

    return false;
}

/*
 * Unify type variables.
 * */
bool unify_variable(Type *v, Type *x, Vector *subst) {
    Type *var = vector_at(subst, v->tvar);
    if (var->kind != T_VAR) {
        return unify(var, x, subst);
    } else if (v->tvar != var->tvar) {
        return unify(var, x, subst);
    } else if (x->kind == T_VAR) {
        Type *xvar = vector_at(subst, x->tvar);
        if (xvar->kind != T_VAR) {
            return unify(v, xvar, subst);
        } else if (xvar->tvar != x->tvar) {
            return unify(v, xvar, subst);
        }
    }

    // If type variable `v` occurs inside type `x`, unify function may unifying infinitely.
    // This avoids that potential.
    if (occurs_check(v, x, subst)) {
        return false;
    }

    // Set type `x` to substitution of type variable `v`.
    vector_set(subst, v->tvar, x);
    return true;
}

// If type variable `v` occurs anywhere inside type `x`, return true.
bool occurs_check(Type *v, Type *x, Vector *subst) {
    if (x->kind == T_VAR && x->tvar == v->tvar) {
        return true;
    } else if (x->kind == T_VAR) {
        Type *xvar = vector_at(subst, x->tvar);
        if (xvar->kind != T_VAR) {
            return occurs_check(v, xvar, subst);
        } else if (xvar->tvar != x->tvar) {
            return occurs_check(v, xvar, subst);
        }
    }

    if (x->kind == T_FUN) {
        bool ret = occurs_check(v, x->ret_type, subst);
        bool arg = false;
        int size = vector_size(x->arg_types);
        for (int i = 0; i < size; i++) {
            Type *arg_ty = vector_at(x->arg_types, i);
            arg |= occurs_check(v, arg_ty, subst);
        }

        return ret || arg;
    }

    return false;
}

Vector *unify_all_equations(TEnv *e, Vector *equations) {
    int size = vector_size(equations);
    Vector *subst = new_vector();
    vector_alloc(subst, e->tvar);
    for (int i = 0; i < e->tvar; i++) {
        Type *var = new_type();
        var->kind = T_VAR;
        var->tvar = i;
        vector_set(subst, i, var);
    }
    for (int i = 0; i < size; i++) {
        TypeEquation *eq = vector_at(equations, i);
        bool ok = unify(eq->lhs, eq->rhs, subst);
        if (!ok)
            return NULL;
    }

    return subst;
}

char *tvar_to_string(int tvar_num) {
    char *s = calloc(1, sizeof(char) * 10);
    s[0] = 't';
    int i = 1;
    while (tvar_num / 10 != 0) {
        s[i++] = (tvar_num % 10) + '0';
        tvar_num /= 10;
    }

    return s;
}

void print_type(Type *t) {
    if (t->kind == T_INT)
        printf("T_INT");
    else if (t->kind == T_BOOL)
        printf("T_BOOL");
    else if (t->kind == T_VAR)
        printf("T_VAR%d", t->tvar);
    else if (t->kind == T_APP)
        printf("T_APP");
    else if (t->kind == T_FUN) {
        printf("(T_FUN (");
        int size = vector_size(t->arg_types);
        for (int i = 0; i < size; i++) {
            Type *argty = vector_at(t->arg_types, i);
            print_type(argty);
            printf(" -> ");
        }

        print_type(t->ret_type);
        printf(")");
        printf(")");
    } else
        printf("unknown type");
}

void print_type_equations(Vector *equations) {
    int size = vector_size(equations);
    for (int i = 0; i < size; i++) {
        TypeEquation *t = vector_at(equations, i);
        print_type(t->lhs);
        printf(" == ");
        print_type(t->rhs);
        printf(" :: ");
        print_node(t->orig);
        printf("\n");
    }
}

void print_all_node_type(Node *nd) {
    print_node(nd);
    printf(" :: ");
    print_type(nd->type);
    printf("\n");

    if (nd->kind == ND_BIN) {
        print_all_node_type(nd->ast.binexpr->lhs);
        print_all_node_type(nd->ast.binexpr->rhs);
    } else if (nd->kind == ND_IF) {
        print_all_node_type(nd->ast.ifexpr->cond);
        print_all_node_type(nd->ast.ifexpr->thenbody);
        print_all_node_type(nd->ast.ifexpr->elsebody);
    } else if (nd->kind == ND_APP) {
        int size = vector_size(nd->ast.appexpr->args);
        for (int i = 0; i < size; i++) {
            Node *arg = vector_at(nd->ast.appexpr->args, i);
            print_all_node_type(arg);
        }
    } else if (nd->kind == ND_FUN) {
        print_all_node_type(nd->ast.funexpr->body);
    } else if (nd->kind == ND_TOPLEVEL) {
        int size = vector_size(nd->ast.toplevel->exprs);
        for (int i = 0; i < size; i++) {
            Node *expr = vector_at(nd->ast.toplevel->exprs, i);
            print_all_node_type(expr);
        }
    }
}

void print_subst(Vector *subst) {
    int size = vector_size(subst);
    for (int i = 0; i < size; i++) {
        Type *ty = vector_at(subst, i);
        printf("T_VAR%d :: ", i);
        print_type(ty);
        printf("\n");
    }
}

Type *get_real_type(Type *t, Vector *subst) {
    if (t->kind == T_FUN) {
        int size = vector_size(t->arg_types);
        for (int i = 0; i < size; i++) {
            Type *arg_ty = vector_at(t->arg_types, i);
            Type *real = get_real_type(arg_ty, subst);
            vector_set(t->arg_types, i, real);
        }

        Type *ret_real = get_real_type(t->ret_type, subst);
        t->ret_type = ret_real;
        return t;
    } else if (t->kind != T_VAR) {
        return t;
    }

    Type *ref_ty = vector_at(subst, t->tvar);
    if (ref_ty->kind == T_VAR && ref_ty->tvar == t->tvar)
        return t;
    Type *real = get_real_type(ref_ty, subst);
    *t = *real;
    return t;
}

void print_typed_node(Node *nd, Vector *subst) {
    if (nd->kind != ND_TOPLEVEL) {
        print_node(nd);
        printf(" :: ");
        print_type(get_real_type(nd->type, subst));
        printf("\n");
    }

    if (nd->kind == ND_BIN) {
        print_typed_node(nd->ast.binexpr->lhs, subst);
        print_typed_node(nd->ast.binexpr->rhs, subst);
    } else if (nd->kind == ND_IF) {
        print_typed_node(nd->ast.ifexpr->cond, subst);
        print_typed_node(nd->ast.ifexpr->thenbody, subst);
        print_typed_node(nd->ast.ifexpr->elsebody, subst);
    } else if (nd->kind == ND_APP) {
        int size = vector_size(nd->ast.appexpr->args);
        for (int i = 0; i < size; i++) {
            Node *arg = vector_at(nd->ast.appexpr->args, i);
            print_typed_node(arg, subst);
        }
    } else if (nd->kind == ND_FUN) {
        print_typed_node(nd->ast.funexpr->body, subst);
    } else if (nd->kind == ND_TOPLEVEL) {
        int size = vector_size(nd->ast.toplevel->exprs);
        for (int i = 0; i < size; i++) {
            Node *expr = vector_at(nd->ast.toplevel->exprs, i);
            print_typed_node(expr, subst);
        }
    }
}
