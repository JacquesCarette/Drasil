#include "ReadTable.h"

#include <stdio.h>
#include <stdlib.h>

#define define_read_table_row_internal(T, FORMAT) \
int read_table_row_internal_##T(FILE* infile, int start, int stop, int step, char delim, T **row){ \
  char c; \
  fpos_t fileStart; \
  fgetpos(infile, &fileStart); \
  \
  int delimCount = 0; \
  int quote = 0; \
  int dataCount = 0; \
  \
  if (stop > 0) { \
    dataCount = (stop - start + 1)/step; \
  } else { \
      do { \
        c = fgetc(infile); \
        if (c == '"') \
          quote = (quote + 1)%2; \
        if (c == delim && quote == 0) \
          delimCount++; \
      } while (c != '\n' && c != EOF); \
    dataCount = (delimCount + 1)/step - start/step; \
  } \
  *row = (T *)malloc(sizeof(T) * dataCount); \
  \
  fsetpos(infile, &fileStart); \
  \
  int currentStep = 0; \
  int index = 0; \
  if (start == 0) { \
    fscanf(infile, FORMAT, *row + index); \
    ++index; \
    ++currentStep; \
  } else \
    --start; \
  do { \
    c = fgetc(infile); \
    if (c == '"') \
      quote = (quote + 1)%2; \
    if (c == delim && quote == 0) { \
      if (start <= 0) { \
        if (currentStep == 0) { \
          fscanf(infile, FORMAT, *row + index); \
          ++index; \
        } \
        currentStep = (currentStep + 1)%step; \
      } \
      --start; \
    } \
  } while (c != '\n' && c != EOF && index < dataCount); \
  \
  while (c != '\n' && c != EOF) c = fgetc(infile); \
  \
  if (index != dataCount) { \
    free(*row); \
    *row = NULL; \
    return -1; \
  } \
  \
  return dataCount; \
}

#define define_read_table_row(T) \
int read_table_row_##T(char* filename, int rowNum, int start, int stop, int step, char delim, T **row){ \
  FILE* infile; \
  infile = fopen(filename, "rb"); \
  char c; \
  while(rowNum > 0){ \
    do { \
      c = fgetc(infile); \
    } while (c != '\n' && c != EOF); \
    if (c == EOF) { \
      *row = NULL; \
      return -1; \
    } \
    --rowNum; \
  } \
  int colCount = read_table_row_internal_##T(infile, start, stop, step, delim, row); \
  fclose(infile); \
  return colCount; \
}

#define define_read_table(T) \
int read_table_##T(char* filename, int skipRows, int start, int stop, int step, char delim, T ***rows){ \
  FILE* infile; \
  infile = fopen(filename, "rb"); \
  \
  char c; \
  while(skipRows > 0){ \
    do { \
      c = fgetc(infile); \
    } while (c != '\n' && c != EOF); \
    if (c == EOF) { \
      *rows = NULL; \
      return -1; \
    } \
    --skipRows; \
  } \
  \
  fpos_t fileStart; \
  fgetpos(infile, &fileStart); \
  \
  int totalRows = 0; \
  do { \
    do { \
      c = fgetc(infile); \
    } while (c != '\n' && c != EOF); \
    ++totalRows; \
  } while (c != EOF); \
  \
  fsetpos(infile, &fileStart); \
  \
  *rows = (T **)malloc(sizeof(T *) * totalRows); \
  for (int i = 0; i < totalRows; i++) { \
    read_table_row_internal_##T(infile, start, stop, step, delim, *rows + i); \
    if (*(*rows + i) == NULL) { \
      --i; \
      --totalRows; \
    } \
  } \
  fclose(infile); \
  return totalRows; \
} 

define_read_table_row_internal(int, "%d")
define_read_table_row_internal(double, "%lf")
define_read_table_row_internal(float, "%f")

define_read_table_row(int)
define_read_table_row(double)
define_read_table_row(float)

define_read_table(int)
define_read_table(double)
define_read_table(float)