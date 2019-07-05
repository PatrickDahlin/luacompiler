#include "tokenizer.h"

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h> // isalnum, isalpha
#include <string.h>



int keyword_count = 21;
char keywords[21][10] = {
	"and","break","do","else","elseif",
	"end","false","for","function","if",
	"in","local","nil","not","or",
	"repeat","return","then","true","until","while"
};

bool eat_char(struct FileHandle* handle, char toeat){
	char c = (char)fgetc(handle->handle);
	if(c != toeat){
		ungetc(c,handle->handle);
	}else{
		handle->cursor_pos++;
	}
	return c == toeat;
}

char peek_char(struct FileHandle* handle){
	char c = (char)fgetc(handle->handle);
	ungetc(c,handle->handle);
	return c;
}

char read_char(struct FileHandle* handle, bool eat_whitespace) {
	char c = '\0';
	do {
		c = (char)fgetc(handle->handle);
		handle->cursor_pos++;
		if(c == '\n'){
			handle->line_nr++;
			handle->line_begin_pos = handle->cursor_pos;
		}
	} while( isspace(c) && eat_whitespace);
	return c;
}

char read_escaped_char(struct FileHandle* handle){
	char c = '\0';
	do {
		c = (char)fgetc(handle->handle);
		handle->cursor_pos++;
		if(c == '\\') {
			char next = (char)fgetc(handle->handle);
			switch(next){
			case 'n': return '\n'; // newline
			case 'a': return '\a'; // bell
			case 't': return '\t'; // tab
			case 'r': return '\r'; // carriage return
			case 'v': return '\v'; // vertical tab
			case 'b': return '\b'; // backspace
			case '\"': return '\"'; // double quote
			case '\'': return '\''; // single quote
			case '\\': return '\\'; // backslash
			case '0': return '\0'; // embedded zero
			default:
				// TODO Handle \ddd format
				error(handle, "invalid escape sequence");
			break;
			}
		}
	} while(true);
	return c;
}

// @unused
int count_chars(struct FileHandle* handle, char ch, bool consume) {
	char c;
	int i = 0;
	int cursor_start = handle->cursor_pos;

	do {
		c = (char)fgetc(handle->handle);
		if(c == ch) i++; else ungetc(c, handle->handle);
	} while(c == ch);

	if(!consume)
		fseek(handle->handle, cursor_start, SEEK_SET);
	else
		handle->cursor_pos += i;

	return i;
}


struct Token parse_integer(struct FileHandle* handle) {
	struct Token t;
	t.type = TOKEN_NUMBER;

	char c = '0';
	c = (char)fgetc(handle->handle);
	handle->cursor_pos++;

	int number = 0;
	float frac = 0;
	int frac_num = 0;
	int sign = 1;
	int e_num = 0;
	if(c == '-'){
		sign = -1;
		c = (char)fgetc(handle->handle);
		handle->cursor_pos++;
	}
	bool detected_e = false;
	bool detected_dot = false;

	if(c == '.'){
		detected_dot = true;
		c = (char)fgetc(handle->handle);
		handle->cursor_pos++;
	}


	while(isdigit(c) || c == 'e' || c == '.') {
		if(c == 'e'){
			if(detected_e) error(handle, "Undefined number constant");
			detected_e = true;
		}
		else if(c == '.'){
			if(detected_dot) error(handle, "Undefined number constant");
			detected_dot = true;
		}
		else
		{
			if(detected_e){
				e_num = e_num * 10 + c - '0';
			}
			else
			{
				if(detected_dot){
					frac_num++;
					frac = frac + (c - '0') * (float)pow(0.1f, frac_num);
				}
				else
				{
					number = number * 10 + c - '0';
				}
			}
		}
		handle->cursor_pos++;
		c = (char)fgetc(handle->handle);
	}

	ungetc(c, handle->handle);
	handle->cursor_pos--;

	//TODO Overflow handling

	t.number = ((float)number + frac) * (float)pow((double)10.0, (double)e_num);
	t.number *= sign;

	return t;
}

/*
	Since strings can start with either " or ' we need to know which one we are parsing
	henceafter we look for the matching end character to end the string
*/
struct Token parse_simple_string(struct FileHandle* handle, char string_start) {
	struct Token t;
	t.type = TOKEN_STRING;
	t.string = NULL;

	char* buffer = (char*)calloc(16,sizeof(char));
	int index = 0, size = 16;
	char prevC = '\0';
	char c = (char)fgetc(handle->handle);
	handle->cursor_pos++;

	while(!(c == string_start && prevC != '\\')){
		if(c == EOF) error(handle, "Encountered end of file before end of string");

		buffer[index] = c;
		index++;
		prevC = c;
		c = (char)fgetc(handle->handle);
		handle->cursor_pos++;
		if(index >= size-1) {
			buffer = (char*)realloc(buffer, size * 2);
			size *= 2;
		}
	}

	buffer[index] = '\0'; index++;

	t.string = (char*)calloc(index-1, sizeof(char));
	strcpy(t.string, &buffer[0]);
	free(buffer);

	return t;
}


/*
	Multiline string parsing, these begin with two [ brackets with
	any number of = between them, where a matching nunmber of equal signs
	match then ending of the string
*/
struct Token parse_multiline_string(struct FileHandle* handle) {
	// We've read in the first bracket which means we can ignore it

	struct Token t;
	t.type = TOKEN_STRING;
	t.string = NULL;

	int equal_count = 0;

	while(eat_char(handle, '=')){ equal_count++; }

	if(!eat_char(handle, '[')) { error(handle, "Expected open string brace"); }

	// Read string until we find end

	char* buffer = (char*)calloc(16, sizeof(char));
	int buf_len = 16;
	int index = 0;
	// If first char is newline, ignore it
	eat_char(handle, '\n'); eat_char(handle, '\r');
	char c = read_char(handle, false);

	while(true) {

		if(c == EOF){ error(handle, "Unclosed string literal"); }

		if(c == ']') {
			int cursor_start = handle->cursor_pos;
			int ending_equals = 0;
			while(eat_char(handle, '=')){ ending_equals++; }

			if(ending_equals == equal_count && eat_char(handle, ']')){
				break;
			}
			// If we reach this point, we've read in a potential end
			// but it didn't match so now we need to put all read
			// characters into the buffer instead
			buffer[index] = c;
			index++;
			if(index >= buf_len){ buf_len *= 2; buffer = (char*)realloc(buffer, buf_len); }
			handle->cursor_pos = cursor_start;
			fseek(handle->handle, cursor_start, SEEK_SET);
			// Putting the beginning bracket into the buffer means we won't
			// go into this end parsing stage, thus continuing the string
		} else {
			buffer[index] = c;
			index++;
			if(index >= buf_len){ buf_len *= 2; buffer = (char*)realloc(buffer, buf_len); }
		}

		c = read_char(handle, false);
	}

	buffer[index] = '\0';
	t.string = (char*)malloc(index);
	strcpy(t.string,buffer);
	free(buffer);

	return t;
}

struct Token parse_float(struct FileHandle* handle) {
	struct Token t;
	t.type = TOKEN_NUMBER;
	error(handle, "Float parsing not implemented");
	return t;
}

struct Token parse_identifier(struct FileHandle* handle) {
	struct Token t;
	t.type = TOKEN_IDENT;
	t.string = NULL;

	char c = read_char(handle, false);
	char buffer[128];
	int index = 0;


	if(!isalpha(c) && c != '_'){
		error(handle, "identifiers must begin with a letter or underscore");
		return t;
	}

	while(isalnum(c) || c == '_') {
		buffer[index] = c;
		index++;
		if(index >= 128) break;
		c = read_char(handle, false);
	}

	if(index < 128) {
		ungetc(c, handle->handle);
		handle->cursor_pos--;
		buffer[index] = '\0';
	}
	else
	{
		buffer[127] = '\0';
	}


	// check if it's a keyword
	for(int i=0; i < keyword_count; i++) {
		if(strcmp(&keywords[i][0], &buffer[0]) == 0) {
			t.type = TOKEN_KEYWORD; // Change to keyword instead of identifier
		}
	}

	if(index < 1) error(handle, "Too small of a string to be able to parse");

	t.string = (char*)malloc(index * sizeof(char));
	strcpy(t.string, &buffer[0]);


	return t;
}
/*
struct Token parse_character(struct FileHandle* handle) {
	struct Token t;
	t.type = CHAR;

	// TODO support special characters such as \n and \t which are two characters parsed to one

	char c = (char)fgetc(handle->handle);
	char c2 = (char)fgetc(handle->handle);

	if(c2 != '\'') {
		error(handle, "character literal can only contain one character");
		return t;
	}

	t.character = c;

	return t;
}
*/

struct Token parse_token(struct FileHandle* handle, bool movecursor) {
	struct Token token;

	// Parse the current token from file input
	int start_cursor = handle->cursor_pos;
	int start_line = handle->line_nr;

	fseek(handle->handle, start_cursor, SEEK_SET);

	// Read character
	char c = read_char(handle, true);
	char peekChar = peek_char(handle);

	switch(c){
	case '*': token.type = TOKEN_STAR; break;
	case '/': token.type = TOKEN_FSLASH; break;
	case '+': token.type = TOKEN_PLUS; break;
	case '%': token.type = TOKEN_PERC; break;
	case '(': token.type = TOKEN_LPAREN; break;
	case ')': token.type = TOKEN_RPAREN; break;
	case '^': token.type = TOKEN_POW; break;
	case '{': token.type = TOKEN_LBRACKET; break;
	case '}': token.type = TOKEN_RBRACKET; break;
	case '\\':token.type = TOKEN_BSLASH; break;
	case ';': token.type = TOKEN_SEMICOLON; break;
	case ':': token.type = TOKEN_COLON; break;
	case '#': token.type = TOKEN_HASH; break;
	case ',': token.type = TOKEN_COMMA; break;
	case EOF: token.type = TOKEN_END_OF_FILE; break;
	case '-':
		token.type = TOKEN_MINUS;
		if(peekChar == '-'){
			while( (c = fgetc(handle->handle) ) != '\n'){}
			return parse_token(handle, movecursor);
		}
	break;
	case '[':
		token.type = TOKEN_LSQBRACKET;
		if(peekChar == '[' || peekChar == '=') {
			token = parse_multiline_string(handle);
		}
	break;
	case ']':
		token.type = TOKEN_RSQBRACKET;
	break;
	case '.':
		// Triple dot is the only three char token in lua hence the special case
		if(peekChar == '.') {
			// We have to fgetc twice since our peek char isn't read yet
			char tmpPeek = (char)fgetc(handle->handle);
			char tmpC = (char)fgetc(handle->handle);
			handle->cursor_pos += 2;
			if(tmpC == '.') {
				token.type = TOKEN_TDOT;
				break;
			}
			token.type = TOKEN_DDOT;
			ungetc(tmpC, handle->handle);
			handle->cursor_pos--;
		} else {
			if(isdigit(peekChar)){
				ungetc(c, handle->handle);
				handle->cursor_pos--;
				token = parse_integer(handle);
			} else
				token.type = TOKEN_DOT;
		}
	break;
	case '=':
		token.type = TOKEN_EQUAL;
		if(peekChar == '=') {
			token.type = TOKEN_DEQUAL;
			eat_char(handle, '=');
		}
	break;
	case '<':
		token.type = TOKEN_LESS;
		if(peekChar == '=') {
			token.type = TOKEN_LEQ;
			eat_char(handle, '=');
		}
	break;
	case '>':
		token.type = TOKEN_GREATER;
		if(peekChar == '=') {
			token.type = TOKEN_GEQ;
			eat_char(handle, '=');
		}
	break;
	case '~':
		if(peekChar != '=') { error(handle,"Expected '=' in expression"); }
		token.type = TOKEN_NOTEQ;
		eat_char(handle, '=');
	break;
	case '\'':
		// NOTE no character literal in lua
		token = parse_simple_string(handle,'\'');
	break;
	case '"':
		token = parse_simple_string(handle,'"');
	break;
	default:
		ungetc(c, handle->handle);
		handle->cursor_pos--;
		if(isdigit(c)) {
			token = parse_integer(handle);
			break;
		}
		if(!isalpha(c) && c != '_') { /* error, identifiers begin with a letter a-zA-Z */
			printf("Encountered token %c beginning of an identifier\n",c);
			error(handle, "Unknown token");
			break;
		}

		token = parse_identifier(handle);
		break;
	}


	if(!movecursor) {
		handle->cursor_pos = start_cursor;
		handle->line_nr = start_line;
	}

	return token;
}


const char* tokenizer_token_str(struct Token t) {
	switch(t.type) {
	case TOKEN_HASH: return "HASH";
	case TOKEN_DOT: return "DOT";
	case TOKEN_DDOT: return "DDOT";
	case TOKEN_TDOT: return "TDOT";
	case TOKEN_PLUS: return "PLUS";
	case TOKEN_MINUS: return "MINUS";
	case TOKEN_EQUAL: return "EQUAL";
	case TOKEN_DEQUAL: return "DEQUAL";
	case TOKEN_STAR: return "STAR";
	case TOKEN_LESS: return "LESS";
	case TOKEN_GREATER: return "GREATER";
	case TOKEN_FSLASH: return "FSLASH";
	case TOKEN_BSLASH: return "BSLASH";
	case TOKEN_RPAREN: return "RPAREN";
	case TOKEN_LPAREN: return "LPAREN";
	case TOKEN_RBRACKET: return "RBRACKET";
	case TOKEN_LBRACKET: return "LBRACKET";
	case TOKEN_RSQBRACKET: return "RSQBRACKET";
	case TOKEN_LSQBRACKET: return "LSQBRACKET";
	case TOKEN_STRING: return "STRING";
	//case CHAR: return "CHAR";
	case TOKEN_NUMBER: return "NUMBER";
	case TOKEN_SEMICOLON: return "SEMICOLON";
	case TOKEN_COLON: return "COLON";
	case TOKEN_IDENT: return "IDENT";
	case TOKEN_KEYWORD: return "KEYWORD";
	case TOKEN_COMMA: return "COMMA";
	case TOKEN_GEQ: return "GEQ";
	case TOKEN_LEQ: return "LEQ";
	case TOKEN_PERC: return "PERC";
	case TOKEN_POW: return "POW";
	case TOKEN_NOTEQ: return "NOTEQ";
	case TOKEN_END_OF_FILE: return "END_OF_FILE";
	default: return "Unknown token type";
	}
}


struct FileHandle* tokenizer_begin(char* filepath) {

	struct FileHandle* handle = (struct FileHandle*)malloc(sizeof(struct FileHandle));

	if(!(handle->handle = fopen(filepath, "r"))) {
		printf("Couldn't find source file %s\n",filepath);
		exit(-1);
		return NULL;
	}

	handle->filename = (char*)calloc(strlen(filepath)+1,sizeof(char));
	strcpy(handle->filename, filepath);

	handle->prev_token_line = 0;
	handle->prev_token_line_pos = 0;
	handle->prev_token_pos = 0;
	handle->line_begin_pos = 0;
	handle->cursor_pos = 0;
	handle->line_nr = 1;
	if(handle->handle != NULL) {
		fseek(handle->handle, 0, SEEK_END);
		handle->file_size = ftell(handle->handle);
		fseek(handle->handle, 0, SEEK_SET);
	}
	else
		handle->file_size = 0;

	handle->current_token = parse_token(handle, true);
	return handle;
}

bool tokenizer_has_next(struct FileHandle* handle) {
	return !(handle->current_token.type == TOKEN_END_OF_FILE &&
		tokenizer_peek_next(handle).type == TOKEN_END_OF_FILE);
}

bool tokenizer_move_next(struct FileHandle* handle) {
	if(!tokenizer_has_next(handle)) return false;
	if(handle->current_token.type == TOKEN_STRING ||
		handle->current_token.type == TOKEN_IDENT ||
		handle->current_token.type == TOKEN_KEYWORD)
		free(handle->current_token.string);

	handle->prev_token_pos = handle->cursor_pos;
	handle->prev_token_line = handle->line_nr;
	handle->prev_token_line_pos = handle->line_begin_pos;
	handle->current_token = parse_token(handle, true);
	return true;
}

struct Token tokenizer_peek_next(struct FileHandle* handle) {
	struct Token token;
	token = parse_token(handle, false);
	return token;
}

void tokenizer_end(struct FileHandle* handle) {
	fclose(handle->handle);
	handle->handle = NULL;
	free(handle->filename);
	handle->filename = NULL;
	if(handle->current_token.type == TOKEN_STRING ||
		handle->current_token.type == TOKEN_IDENT ||
		handle->current_token.type == TOKEN_KEYWORD)
		free(handle->current_token.string);

}

