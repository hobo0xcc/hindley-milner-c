#include "hm.h"

Token *new_token(int ty, char *str) {
    Token *tk = malloc(sizeof(Token));
    tk->ty = ty;
    tk->str = str;
    return tk;
}

char *substr(char *start, char *end) {
    char *res = calloc(1, end - start + 1);
    int i = 0;
    while (start < end) {
        res[i] = *start;
        start++; i++;
    }

    return res;
}

Token *next(char **p) {
    char *cur = *p;
    Token *result;
    bool is_tokenized = false;

    while (*cur) {
        if (isspace(*cur)) {
            cur++;
            continue;
        }
        if (isdigit(*cur)) {
            char *start = cur;
            while (*cur && isdigit(*cur)) cur++;
            char *str = substr(start, cur);
            result = new_token(TK_INT, str);
            is_tokenized = true;
            break;
        }

        if (isalpha(*cur)) {
            char *start = cur;
            while (*cur && isalnum(*cur)) cur++;
            char *str = substr(start, cur);
            int ty = TK_IDENT;
            if (strcmp(str, "if") == 0)      ty = TK_IF;
            if (strcmp(str, "then") == 0)    ty = TK_THEN;
            if (strcmp(str, "else") == 0)    ty = TK_ELSE;
            if (strcmp(str, "fun") == 0)     ty = TK_FUN;
            if (strcmp(str, "true") == 0)    ty = TK_TRUE;
            if (strcmp(str, "false") == 0)   ty = TK_FALSE;

            result = new_token(ty, str);
            is_tokenized = true;
            break;
        }

        int ty = TK_ILLEGAL;
        static char dummy[1];
        dummy[0] = 0;
        switch (*cur) {
            case '+': ty = TK_PLUS;     cur++; break;
            case '-': ty = TK_MINUS;    cur++; break;
            case '*': ty = TK_ASTERISK; cur++; break;
            case '/': ty = TK_SLASH;    cur++; break;
            case '(': ty = TK_LPAREN;   cur++; break;
            case ')': ty = TK_RPAREN;   cur++; break;
            case ',': ty = TK_COMMA;    cur++; break;
            case '!':
                if (*(cur + 1) == '=') {
                    ty = TK_NEQ;
                    cur += 2;
                } else {
                    error("Unknown token: %c", *cur);
                }
            case '=':
                if (*(cur + 1) == '=') {
                    ty = TK_EQQ;
                    cur += 2;
                } else {
                    ty = TK_EQUAL;
                    cur++;
                }
            default:  break;
        }

        is_tokenized = true;
        if (ty == TK_ILLEGAL) {
            result = new_token(ty, dummy);
            cur++;
            break;
        } else {
            result = new_token(ty, dummy);
            break;
        }
    }

    if (is_tokenized) {
        *p = cur;
        return result;
    } else {
        static char eof[1];
        eof[0] = 0;
        return new_token(TK_EOF, eof);
    }
}

Vector *tokenize(char *src) {
    Vector *tokens = new_vector();
    for (Token *tk = next(&src); tk->ty != TK_EOF; tk = next(&src)) {
        if (tk->ty == TK_ILLEGAL) {
            error("illegal token");
        }

        vector_push_back(tokens, tk);
    }

    return tokens;
}

char *type_to_string(int ty) {
    char *str;
    switch (ty) {
        case TK_IDENT:      str = "IDENT"; break;
        case TK_INT:        str = "INT"; break;
        case TK_TRUE:       str = "TRUE"; break;
        case TK_FALSE:      str = "FALSE"; break;
        case TK_IF:         str = "IF"; break;
        case TK_THEN:       str = "THEN"; break;
        case TK_ELSE:       str = "ELSE"; break;
        case TK_FUN:        str = "FUN"; break;
        case TK_PLUS:       str = "PLUS"; break;
        case TK_MINUS:      str = "MINUS"; break;
        case TK_ASTERISK:   str = "ASTERISK"; break;
        case TK_SLASH:      str = "SLASH"; break;
        case TK_EQUAL:      str = "EQUAL"; break;
        case TK_EQQ:        str = "EQQ"; break;
        case TK_NEQ:        str = "NEQ"; break;
        case TK_LPAREN:     str = "LPAREN"; break;
        case TK_RPAREN:     str = "RPAREN"; break;
        case TK_COMMA:      str = "COMMA"; break;
        case TK_ILLEGAL:    str = "ILLEGAL"; break;
        case TK_EOF:        str = "EOF"; break;
        default:            str = "ILLEGAL"; break;
    }

    return str;
}
