#ifndef READTABLE_H
#define READTABLE_H

#define read_table_row(T) read_table_row_##T
#define read_table(T) read_table_##T

#define declare_read_table_row(T) \
int read_table_row_##T(char* filename, int rowNum, int start, int stop, int step, char delim, T** row);

#define declare_read_table(T) \
int read_table_##T(char* filename, int skipRows, int start, int stop, int step, char delim, T*** rows);

declare_read_table_row(int)
declare_read_table_row(double)
declare_read_table_row(float)

declare_read_table(int)
declare_read_table(double)
declare_read_table(float)

#endif /* READTABLE_H */