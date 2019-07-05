#pragma once

#include <stdio.h>
#include "common.h"

enum TokenType {
	TOKEN_PLUS,		// +
	TOKEN_LESS,		// <
	TOKEN_GREATER,		// >
	TOKEN_MINUS,		// -
	TOKEN_EQUAL,		// =
	TOKEN_DEQUAL,		// ==
	TOKEN_GEQ,		// >=
	TOKEN_LEQ,		// <=
	TOKEN_PERC,		// %
	TOKEN_POW,		// ^
	TOKEN_STAR,		// *
	TOKEN_FSLASH,		// /
	TOKEN_NOTEQ,		// ~=
	TOKEN_DOT,		// .
	TOKEN_DDOT,		// ..
	TOKEN_TDOT,		// ...
	TOKEN_HASH,		// #
	TOKEN_BSLASH,		// backslash
	TOKEN_RPAREN,		// )
	TOKEN_LPAREN,		// (
	TOKEN_RBRACKET,		// }
	TOKEN_LBRACKET,		// {
	TOKEN_RSQBRACKET,	// ]
	TOKEN_LSQBRACKET,	// [
	TOKEN_STRING,		// string literal
	//CHAR,		// Character literal !doesn't exist in lua!
	//INT,		// Integer literal
	//FLOAT,		// Float literal
	TOKEN_NUMBER,		// Number literal, float
	TOKEN_SEMICOLON,	// ;
	TOKEN_COLON,		// :
	TOKEN_COMMA,		// ,
	TOKEN_KEYWORD,		// any lua keyword
	TOKEN_IDENT,		// user defined identifier or keyword
	TOKEN_END_OF_FILE
};





struct Token {

	enum TokenType type;

	union {
		char* string;
		float number;
	};
};


struct FileHandle {
	FILE* handle;
	char* filename;
	uint file_size;
	uint cursor_pos;
	uint line_begin_pos;
	uint line_nr;
	uint prev_token_pos;
	uint prev_token_line_pos;
	uint prev_token_line;
	struct Token current_token;
};




// Opens up file for reading and returns a FileHandle struct if successful
// NULL otherwise
struct FileHandle* tokenizer_begin(char* filepath);

// Returns true while there is a token after current one, doesn't move to next one
bool tokenizer_has_next(struct FileHandle* handle);

// Moves to the next token, false if there is no token after current one
bool tokenizer_move_next(struct FileHandle* handle);

// Peek the next token in the file, leaving handle unchanged
struct Token tokenizer_peek_next(struct FileHandle* handle);

void tokenizer_end(struct FileHandle* handle);

const char* tokenizer_token_str(struct Token t);





