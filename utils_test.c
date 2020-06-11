#include "hm.h"

void test_vector() {
    Vector *vec = new_vector();
    int a, b, c;
    a = 1; b = 2; c = 3;
    vector_push_back(vec, (void *)&a);
    vector_push_back(vec, (void *)&b);
    vector_push_back(vec, (void *)&c);

    assert(*(int *)vector_at(vec, 0) == 1);
    assert(*(int *)vector_at(vec, 1) == 2);
    assert(*(int *)vector_at(vec, 2) == 3);
}

void test_map() {
    Map *mp = new_map();
    int a, b;
    a = 10;
    b = 20;
    map_set(mp, "Hello", &a);
    map_set(mp, "Goodbye", &b);
    assert(*(int *)map_get(mp, "Hello") == 10);
    assert(*(int *)map_get(mp, "Goodbye") == 20);
}
