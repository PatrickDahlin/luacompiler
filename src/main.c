#include <stdio.h>
#include "tokenizer.h"
#include "parser.h"
#include "emit_lua.h"
#include "IRParser.h"

#include <assert.h>

void print_token(struct Token t) {

	printf("%s\t", tokenizer_token_str(t));

	switch(t.type) {
	case TOKEN_STRING: printf("(%s)\n", t.string); break;
	case TOKEN_NUMBER: printf("(%f)\n", t.number); break;
	case TOKEN_KEYWORD: case TOKEN_IDENT: printf("(%s)\n", t.string); break;
	case TOKEN_END_OF_FILE: printf("\n"); break;
	default: printf("()\n"); break;
	}
}

int main(int argc, char** argv) {
	printf("Lua tokenizer\n");

	struct FileHandle* source = NULL;

	if(argc == 1) return 0;

	for(int i=1; i < argc; i++) {

		source = tokenizer_begin(argv[i]);
		printf("Token stream for %s>\n",argv[i]);

		while(tokenizer_has_next(source)){
			print_token(source->current_token);
			tokenizer_move_next(source);
		}
		tokenizer_end(source);
	}

	if(argc < 1) { printf("No file to parse\n"); return 0; }

	printf("Parsing %s\n",argv[1]);
	source = NULL;
	source = tokenizer_begin(argv[1]);

	struct Block* blk = parse_AST(source);

	print_AST(blk);

	tokenizer_end(source);

	emit_lua_source("emit_lua.lua", blk);

	printf("File AST emitted as lua into emit_lua.lua\n");
	printf("Parsing IR from AST\n");

	IRNode* ir = ParseIR(blk);

	printf("IRParsing completed\n");

	return 0;
}
