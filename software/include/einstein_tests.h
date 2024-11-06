#ifndef EINSTEIN_TESTS_H
#define EINSTEIN_TESTS_H

#include <stdio.h>
#include <stdint.h>

#ifdef BAREMETAL
#undef assert
#define assert(expr) \
    if (!(expr)) { \
      printf("Failed assertion: " #expr "\n  " __FILE__ ":%u\n", __LINE__); \
      exit(1); \
    }
#endif

bool buffers_are_equal(const void * a, const void * b, size_t size) {
    const bool same_alignment = ((size_t)a) % sizeof(int64_t) == ((size_t)b) % sizeof(int64_t);

    if (same_alignment)
        while (((size_t)a) % sizeof(int64_t) != 0 && ((size_t)b) % sizeof(int64_t) != 0 && size > 0) {
            // Increment the pointers till they both align along a 64-bit boundary (Rocket seems to error out otherwise)
            if (*((int8_t*)a) != *((int8_t*)b))
                return false;
            a = ((int8_t*)a) + 1;
            b = ((int8_t*)b) + 1;
            size--;
        }

    const size_t fast_section = same_alignment ? (size / sizeof(int64_t)) * sizeof(int64_t) : 0;

    for (size_t i = 0; i < fast_section; i += sizeof(int64_t)) {
        const int64_t x = *((int64_t*)((int8_t*)(a) + i));
        const int64_t y = *((int64_t*)((int8_t*)(b) + i));
        if (x != y)
            return false;
    }

    for (size_t i = fast_section; i < size; i++) {
        const int8_t x = *((int8_t*)(a) + i);
        const int8_t y = *((int8_t*)(b) + i);
        if (x != y)
            return false;
    }

    return true;
}

#endif

