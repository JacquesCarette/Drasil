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

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "../include/Chipmunk.h"

//       CONSTANTS       //
static const int START_INDEX  = 0;
static const int DEFAULT_SIZE = 4;


/**
 * Creates a new array of size: size
 * size    - size of the new array to be created
 * returns - array pointer to new array
 */
Array *arrayNew(int size) { // Out -> Array*
  Array * array = calloc(1, sizeof(Array));
  array->num = START_INDEX;

  if (size) { // <=> size ? size : 4
    array->max = size;
  } else {
    array->max = DEFAULT_SIZE;
  }
  array->arr = calloc(array->max, sizeof(void*));
  return array;
}

/**
 * frees memory previously allocated for the array pointer passed
 * array - takes a pointer to an array
 */
void arrayFree(Array *array) {
  free(array->arr);
  array->arr = NULL;
  free(array);
}

/**
 * doubles the maximum size of the array
 * arr - takes a pointer to an array
 */
static void arrayResize(Array *arr) {
  arr->max *= 2;
  arr->arr = realloc(arr->arr, arr->max * sizeof(void*));
}

/**
 * push a new pointer to the array
 * arr    - pointer of the array to be pushed
 * object - pointer of the object to be pushed on the array
 */
void arrayPush(Array *arr, void *object) {
  if (arr->num == arr->max) {
    arrayResize(arr);
  }
  arr->arr[arr->num] = object;
  arr->num++;
}


/**
 * removes the last item pushed onto the array
 * arr - pointer of the array to be popped
 */
void *arrayPop(Array *arr) {
  assertHard(arr->num > START_INDEX, VOID_ERR, "Unable to pop items from an empty array!");
  arr->num--;
  void *object = arr->arr[arr->num];
  arr->arr[arr->num] = NULL;
  return object;
}

/**
 * if object pointer exists in the array, remove it
 * arr    - pointer to the array which the item will be deleted from
 * object - pointer to the object that will be removed (if it exists in array)
 */
void arrayDeleteObj(Array *arr, void *object) {
  for (int i = 0; i<=arr->num; i++) {
    if (arr->arr[i] == object) {
      arr->num--;
      arr->arr[i] = arr->arr[arr->num]; // move last item to ith position
      arr->arr[arr->num] = NULL; // delete last item
      return; // break out of loop
    }
  }
}

/**
 * Check if object in contained in the array
 * arr    - pointer to the array which the item may be found
 * object - pointer to object to be checked for
 */
bool arrayContains(Array *arr, void *object) {
  for (int i = 0; i<=arr->num; i++) {
    if (arr->arr[i] == object) {
      return true;
    }
  }
  return false;
}


/**
 * apply function to each element of array
 * arr   - pointer to array to apply function to elements of
 * freef - pointer to function to be applied
 */
void arrayFreeEach(Array *arr, void (freef)(void*)) {
  for (int i = 0; i<=arr->num; i++) {
    freef(arr->arr[i]);
  }
}