#include "hm.h"

Vector *new_vector() {
    Vector *vec = malloc(sizeof(Vector));
    vec->capacity = sizeof(void *) * 2;
    vec->size = 0;
    vec->data = malloc(vec->capacity);
    return vec;
}

bool vector_grow(Vector *vec, int size) {
    int expected_size = (size + vec->size) * sizeof(void *);
    if (vec->capacity >= expected_size) return true;
    while (vec->capacity < expected_size) vec->capacity *= 2;
    vec->data = realloc(vec->data, vec->capacity);
    if (!vec->data) return false;
    return true;
}

bool vector_push_back(Vector *vec, void *item) {
    bool is_allocated = vector_grow(vec, 1);
    if (!is_allocated) return false;
    vec->data[vec->size++] = item;
    return true;
}

bool vector_set(Vector *vec, int index, void *item) {
    if (index >= vec->size) return false;
    vec->data[index] = item;
    return true;
}

void *vector_at(Vector *vec, int index) {
    if (index >= vec->size) return NULL;
    return vec->data[index];
}

void vector_alloc(Vector *vec, int size) {
    vector_grow(vec, size);
    vec->size += size;
}

int vector_size(Vector *vec) {
    return vec->size;
}

// Map
Map *new_map() {
    Map *mp = malloc(sizeof(Map));
    mp->items = new_vector();
    mp->keys = new_vector();
    mp->len = 0;
    return mp;
}

void map_set(Map *mp, char *key, void *item) {
    vector_push_back(mp->keys, key);
    vector_push_back(mp->items, item);
    mp->len++;
}

void *map_get(Map *mp, char *key) {
    for (int i = 0; i < map_len(mp); i++) {
        char *cmp_key = vector_at(mp->keys, i);
        if (strcmp(cmp_key, key) == 0) {
            return vector_at(mp->items, i);
        }
    }

    return NULL;
}

int map_len(Map *mp) {
    return mp->len;
}
