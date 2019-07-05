#pragma once

typedef unsigned int uint;
typedef unsigned short ushort;
typedef int bool;

#define null NULL
#define false 0
#define true 1
#define SSIZE(x) sizeof(struct x)
#define error(x, msg) { printf("error[%s:%d] %s\n",x->filename, x->prev_token_line, msg);\
char* buf = (char*)malloc(128 * sizeof(char));size_t bufsize = 128;\
fseek(x->handle, x->prev_token_line_pos, SEEK_SET);\
getline(&buf,&bufsize,x->handle);\
printf("%s",buf);for(;x->prev_token_line_pos < x->prev_token_pos;x->prev_token_line_pos++)printf("_");\
printf("^\n");free(buf); exit(-1); }
