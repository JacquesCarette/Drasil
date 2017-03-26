/* Copyright (c) 2013 Scott Lembcke and Howling Moon Software
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

 // ARRAY_C - ARRAY MODULE //

#include <string.h>
#include <stdlib.h>

#include "../include/Chipmunk.h"

Array *arrayNew(int size) {
    Array *arr = (Array *) calloc(1, sizeof(Array));

    arr->num = size;
    arr->max = (size ? size : 4);
    arr->arr = (void **) calloc(arr->max, sizeof(void*));

    return arr;
}

void arrayFree(Array *arr) {
    if (arr) {
        free(arr->arr);
        arr->arr = NULL;

        free(arr);
    }
}

void arrayPush(Array *arr, void *object) {
    if (arr->num == arr->max) {
        arr->max *= 2;
        arr->arr = (void **) realloc(arr->arr, arr->max * sizeof(void*));
    }

    arr->arr[arr->num] = object;
    arr->num++;
}

void *arrayPop(Array *arr) {
    assertHard(arr->num > 0, VOID_ERR, "Unable to pop items from an empty array!");
    arr->num--;

    void *retval = arr->arr[arr->num];
    arr->arr[arr->num] = NULL;

    return retval;
}

void arrayDeleteObj(Array *arr, void *object) {
    for (int i = 0; i < arr->num; ++i) {
        if (arr->arr[i] == object) {
            --arr->num;

            arr->arr[i] = arr->arr[arr->num];
            arr->arr[arr->num] = NULL;

            return;
        }
    }
}

bool arrayContains(Array *arr, void *ptr) {
    for (int i = 0; i < arr->num; ++i) {
        if (arr->arr[i] == ptr) return true;
    }

    return false;
}

void arrayFreeEach(Array *arr, void (freeFunc)(void*)) {
    for (int i = 0; i < arr->num; ++i) {
        freeFunc(arr->arr[i]);
    }
}
