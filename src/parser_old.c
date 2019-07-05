/*


var = has to begin with IDENT following '[' exp ']' or '.'


if(!NAME) return null;
while(NAME) parse_name()
if '(' parse_func()
if '[' parse_exp() eat ']'



var <-> prefix <-> funccall -> explist -> exp -> prefix


*/
#if false

#include "parser.h"
#include "tokenizer.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG_PRINT

void dlog(const char* str){
#ifdef DEBUG_PRINT
	printf("[DEBUG] %s\n",str);
#endif
}

#undef DEBUG_PRINT


typedef struct {
	int cursor_pos;
	int linenr;
	struct Token token;
	char* token_str;
} FileMark;

FileMark set_mark(struct FileHandle* src){
	FileMark mark;
	mark.cursor_pos = src->cursor_pos;
	mark.linenr = src->line_nr;
	mark.token = src->current_token;
	mark.token_str = NULL;

	if(mark.token.type != TOKEN_IDENT &&
		mark.token.type != TOKEN_STRING &&
		mark.token.type != TOKEN_KEYWORD) return mark;

	if(mark.token.string == NULL) return mark;

	// Copy token string since it's lost when token is consumed
	mark.token_str = calloc(strlen(src->current_token.string)+1,sizeof(char));
	strcpy(mark.token_str, src->current_token.string);
	return mark;
}

void free_mark(FileMark* mark){
	if(!mark) return;
	free(mark->token_str);
}

void goto_mark(struct FileHandle* src, FileMark mark){
	src->cursor_pos = mark.cursor_pos;
	src->line_nr = mark.linenr;
	src->current_token = mark.token;
	// Copy over string if one exists
	if(mark.token.type != TOKEN_IDENT &&
		mark.token.type != TOKEN_STRING &&
		mark.token.type != TOKEN_KEYWORD) return;

	if(mark.token_str == NULL) return;

	src->current_token.string = calloc(strlen(mark.token_str)+1,sizeof(char));
	strcpy(src->current_token.string, mark.token_str);
}



// Forward declarations
struct Block* parse_block(struct FileHandle* src);
struct Expression* parse_exp(struct FileHandle* src);
struct Expression_List* parse_exp_list(struct FileHandle* src);
struct Function_Call* parse_func_call(struct FileHandle* src, bool nullable, bool skip_prefix);
struct Var_Decl_List* parse_var_list(struct FileHandle* src);
struct Variable_Decl* parse_var(struct FileHandle* src, bool nullable);
struct Args* parse_args(struct FileHandle* src);


/*
 * Eat given token, returns false if token wasn't found
 * and will only move file cursor if current token is the same token
*/
bool eat_token(struct FileHandle* src, enum TokenType t){
	if(src->current_token.type != t) return false;
	tokenizer_move_next(src);
	return true;
}

/*
 * Eat token with string value, case-sensitive
 *
*/
bool eat_str_token(struct FileHandle* src, enum TokenType t, char* str) {
	if(src->current_token.type != t) return false;
	if(strcmp(src->current_token.string, str) != 0) return false;
	tokenizer_move_next(src);
	return true;
}

/*
 * OK
*/
struct Name_List* parse_name_list(struct FileHandle* src) {
	struct Name_List* names = NULL;
	struct Token token = src->current_token;
	int name_count = 0;

	while(token.type == TOKEN_IDENT) {

		if(!names) names = malloc(SSIZE(Name_List));

		// resize names list
		name_count++;
		names->names = realloc(names->names, sizeof(char*) * name_count);

		// Copy over the string name
		names->names[name_count-1] = (char*)calloc(strlen(token.string)+1, sizeof(char));
		strcpy(names->names[name_count-1], token.string);

		dlog("[namelist] parsed name");

		eat_token(src, TOKEN_IDENT);
		token = src->current_token;
		if(token.type != TOKEN_COMMA) break;
	}

	return names;
}

/*
 * Parameters can be either namelist, namelist and a varargs or just varargs
 * Returns NULL if no names nor varargs were found
 *
 * OK
*/
struct Parameter_List* parse_param_list(struct FileHandle* src){

	struct Name_List* names = parse_name_list(src);
	struct Parameter_List* params = malloc(SSIZE(Parameter_List));

	struct Token token = src->current_token;
	if(token.type == TOKEN_TDOT)
		params->varargs_end = true;
	else
		params->varargs_end = false;

	bool has_varargs = eat_token(src, TOKEN_TDOT);

	if(names == NULL && !has_varargs) return NULL;

	dlog("[param list] successfully parsed parameters");

	return params;
}

/*
 * Function declaration can be of multiple types, here we parse closures
 * noted as 'function' in BNF list
 *
 * OK
*/
struct Function_Closure* parse_func_closure(struct FileHandle* src) {

	struct Token token = src->current_token;

	// We're expecting 'function' keyword
	if(!eat_str_token(src, TOKEN_KEYWORD, "function")) return NULL;

	// Parameter parsing
	if(!eat_token(src, TOKEN_LPAREN)) error(src, "Expected parameter list");
	// Here we are expecting parameter list, nothing else
	// Error if we can't find it

	struct Function_Closure* func = malloc(SSIZE(Function_Closure));
	func->params = parse_param_list(src);
	func->block = parse_block(src);
	if(!eat_str_token(src, TOKEN_KEYWORD, "end")) error(src, "Unclosed function scope");

	dlog("[func closure] parsed successfully");

	return func;
}



struct Prefix_Exp* parse_prefix_exp(struct FileHandle* src, bool nullable);
struct Table* parse_table_cons(struct FileHandle* src);
struct Binary_Op* parse_bin_op(struct FileHandle* src);
struct Unary_Op* parse_un_op(struct FileHandle* src);

/*
 * OK
*/
struct Expression* parse_exp(struct FileHandle* src) {

	struct Expression* expr = calloc(1, SSIZE(Expression));
	struct Token token = src->current_token;

	printf("[exp] parsing expression in parse_exp\n");
	printf("\tcurrent token is: %s\n", tokenizer_token_str(token));

	bool parsed = false;


	struct Prefix_Exp* pre = parse_prefix_exp(src,true);
	if(pre != NULL){
		dlog("Expression is a prefix expression");
		expr->type = EXP_PREFIX_EXP;
		expr->prefix_exp = pre;
		return expr;
	}


	switch(token.type){
	case TOKEN_STRING:
		printf("[DEBUG] String expression: %s\n",token.string);
		expr->type = EXP_STRING;
		expr->string = calloc(strlen(token.string)+1,sizeof(char));
		strcpy(expr->string, token.string);
		parsed = true;
		eat_token(src, TOKEN_STRING);
	break;
	case TOKEN_NUMBER:
		printf("[DEBUG] Number expression: %f\n", token.number);
		expr->type = EXP_NUMBER;
		expr->number = token.number;
		parsed = true;
		eat_token(src, TOKEN_NUMBER);
	break;
	case TOKEN_TDOT:
		dlog("Expression is varargs: '...'");
		expr->type = EXP_VARARGS;
		parsed = true;
		eat_token(src, TOKEN_TDOT);
	break;
	case TOKEN_KEYWORD:
		if(strcmp(token.string, "nil") == 0){
			dlog("nil expression");
			expr->type = EXP_NIL;
			parsed = true;
			eat_token(src, TOKEN_KEYWORD);
		}else if(strcmp(token.string, "false") == 0){
			dlog("boolean false value expression");
			expr->type = EXP_BOOL;
			expr->bool_value = false;
			parsed = true;
			eat_token(src, TOKEN_KEYWORD);
		}else if(strcmp(token.string, "true") == 0) {
			dlog("boolean true value expression");
			expr->type = EXP_BOOL;
			expr->bool_value = true;
			parsed = true;
			eat_token(src, TOKEN_KEYWORD);
		}
	break;
	default: break;
	}

	// Left to parse; Func closure, prefixexp, tablecons, binop, unop

	if(parsed) return expr;

	struct Function_Closure* func = parse_func_closure(src);
	if(func != NULL) {
		dlog("Expression is a function closure");
		expr->type = EXP_FUNC_CLO;
		expr->func_closure = func;
		return expr;
	}

	struct Table* table = parse_table_cons(src);
	if(table != NULL) {
		dlog("Expression is a table");
		expr->type = EXP_TABLE;
		expr->table = table;
		return expr;
	}

	struct Binary_Op* bin_op = parse_bin_op(src);
	if(bin_op != NULL) {
		dlog("Expression is a binary operator");
		expr->type = EXP_BIN_OP;
		expr->binary_op = bin_op;
		return expr;
	}

	struct Unary_Op* un_op = parse_un_op(src);
	if(un_op != NULL) {
		dlog("Expression is an unary operator");
		expr->type = EXP_UN_OP;
		expr->unary_op = un_op;
		return expr;
	}


	// TODO if we reach this point we couldn't parse an expression from here
	error(src, "Expected expression");
	return NULL;
}

/*
 * OK
*/
struct Field* parse_field(struct FileHandle* src) {

	struct Token token = src->current_token;
	struct Token peek = tokenizer_peek_next(src);
	// Make sure we NULL all pointers with calloc
	struct Field* field = calloc(1, SSIZE(Field));

	// '[' expr ']' '=' expr
	if(token.type == TOKEN_LSQBRACKET) {
		field->left = parse_exp(src);
		if(!eat_token(src, TOKEN_RSQBRACKET)) error(src, "Expected closing square bracket");
		if(!eat_token(src, TOKEN_EQUAL)) error(src, "Expected equal sign to complete field");
		field->right = parse_exp(src);
		return field;
	}
	else if(token.type == TOKEN_IDENT && peek.type == TOKEN_EQUAL)
	{
		field->name = calloc(strlen(token.string)+1,sizeof(char));
		strcpy(field->name, token.string);
		eat_token(src, TOKEN_IDENT);
		token = src->current_token;
		eat_token(src, TOKEN_EQUAL);
		field->right = parse_exp(src);
		if(!field->right) error(src, "Expected expression");
		return field;
	}

	field->right = parse_exp(src);
	return field;
}

/*
 * Parse 0..N fields and store in field list
 * Field list can end in (COMMA | SEMICOLON) or a field
 * Returns NULL if no fields can be parsed
 *
 * OK
*/
struct Field_List* parse_field_list(struct FileHandle* src) {

	// Try parse a field
	struct Field* f = NULL;
	int field_c = 0;

	// Field list can end on a separator or a field
	while(true) {
		struct Field* tmp = parse_field(src);
		if(!tmp) break; // Ends with separator
		field_c++;
		f = realloc(f, SSIZE(Field) * field_c);
		memcpy(&f[field_c-1],tmp,SSIZE(Field));
		free(tmp);

		// Ends with field
		if(!eat_token(src, TOKEN_COMMA) && !eat_token(src, TOKEN_SEMICOLON)) break;
	}
	if(field_c == 0) error(src, "Expected one or more fields");

	struct Field_List* out = malloc(SSIZE(Field_List));
	out->fields = f;
	out->field_count = field_c;
	return out;
}

/*
 * Tables are pretty much same as field list with the exception
 * of them being enclosed in braces
 *
 * 'Table' is just defined for readability as a macro that
 * gets replaced with 'Field_List' since they're identical
 * (This only applies to anonymous tables)
 *
 * OK
*/
struct Table* parse_table_cons(struct FileHandle* src) {

	if(!eat_token(src, TOKEN_LBRACKET)) return NULL;

	// Field_List and Table are the same struct
	struct Table* t = parse_field_list(src);

	if(!eat_token(src, TOKEN_RBRACKET)) error(src, "Expected closing brace for table constructor");


	dlog("Parsed table constructor");
	return t;

}

/*
 * OK
*/
struct Func_Name* parse_func_name(struct FileHandle* src) {

	if(src->current_token.type != TOKEN_IDENT) error(src,"Expected function name");

	// Store first identifier in names list
	char** names = calloc(1, sizeof(char*));
	int name_c = 1;
	names[name_c-1] = calloc(strlen(src->current_token.string)+1,sizeof(char));
	strcpy(names[name_c-1], src->current_token.string);

	struct Func_Name* funcn = calloc(1, SSIZE(Func_Name));

	while(true){
		if(!eat_token(src, TOKEN_DOT)) {
			if(!eat_token(src, TOKEN_COLON)) break; // Found end of namelist
			funcn->lastname = calloc(strlen(src->current_token.string)+1,sizeof(char));
			strcpy(funcn->lastname, src->current_token.string);
			break;// last NAME identifier marks end of func name list
		}
		else {
			if(src->current_token.type != TOKEN_IDENT) error(src, "Expected identifer");
			name_c++;
			names = realloc(names,name_c * sizeof(char*));
			names[name_c-1] = calloc(strlen(src->current_token.string)+1,sizeof(char));
			strcpy(names[name_c-1], src->current_token.string);
		}
	}


	printf("[DEBUG] Parsed function name beginning with %s\n", funcn->names[0]);

	funcn->names = names;
	funcn->name_c = name_c;
	return funcn;
}

/*
 * OK
*/
struct Unary_Op* parse_un_op(struct FileHandle* src) {

	struct Token token = src->current_token;

	struct Unary_Op* un = malloc(SSIZE(Unary_Op));

	switch(token.type){
	case TOKEN_HASH:
		un->op = UOP_HASH;
	break;
	case TOKEN_MINUS:
		un->op = UOP_NEG;
	break;
	case TOKEN_IDENT:
		if(strcmp(token.string, "not") == 0){
			un->op = UOP_NOT;
			break;
		}
	default:
		free(un); return NULL;
	}

	un->right = parse_exp(src);

	if(!un->right) error(src, "Expected expression");
	return un;
}


/*
 * OK
*/
struct Binary_Op* parse_bin_op(struct FileHandle* src) {


	struct Expression* left = parse_exp(src);
	if(!left) return NULL;

	enum Bin_Op op;
	switch(src->current_token.type){
	case TOKEN_PLUS:	op = BOP_ADD; break;
	case TOKEN_MINUS:	op = BOP_SUB; break;
	case TOKEN_STAR:	op = BOP_MUL; break;
	case TOKEN_FSLASH:	op = BOP_DIV; break;
	case TOKEN_POW:		op = BOP_POW; break;
	case TOKEN_PERC:	op = BOP_MOD; break;
	case TOKEN_DDOT:	op = BOP_APP; break;
	case TOKEN_LESS:	op = BOP_LES; break; // <
	case TOKEN_GREATER:	op = BOP_GRE; break; // >
	case TOKEN_LEQ:		op = BOP_LEQ; break; // <=
	case TOKEN_GEQ:		op = BOP_GEQ; break; // >=
	case TOKEN_DEQUAL:	op = BOP_EQA; break; // ==
	case TOKEN_NOTEQ:	op = BOP_NEQ; break; // ~=
	case TOKEN_KEYWORD: // and, or (Don't accept anything else)
		if(strcmp(src->current_token.string, "and"))
		{ op = BOP_AND; break; }
		if(strcmp(src->current_token.string, "or"))
		{ op = BOP_OR; break; }
		return NULL;
	default:
		return NULL;
	}

	struct Expression* right = parse_exp(src);
	if(!right) return NULL;

	dlog("[Parsed binary operator]");

	struct Binary_Op* bin_op = malloc(SSIZE(Binary_Op));
	bin_op->left = left;
	bin_op->right = right;
	bin_op->op = op;
	return bin_op;
}

struct While_Block* parse_while_block(struct FileHandle* src){

	struct While_Block* block = malloc(SSIZE(While_Block));

	if(!eat_str_token(src, TOKEN_KEYWORD, "while")) return NULL;

	block->exp = parse_exp(src);

	if(!block->exp) error(src, "Expected expression");

	if(!eat_str_token(src, TOKEN_KEYWORD, "do")) error(src, "Expected do block");

	block->block = parse_block(src); // :3

	if(!eat_str_token(src, TOKEN_KEYWORD, "end")) error(src, "Expected end of do block");

	dlog("[Parsed while block]");

	return block;

}

struct While_Block* parse_repeat_block(struct FileHandle* src){

	struct While_Block* block = malloc(SSIZE(While_Block));

	if(!eat_str_token(src, TOKEN_KEYWORD, "repeat")) return NULL;

	block->block = parse_block(src);

	if(!eat_str_token(src, TOKEN_KEYWORD, "until")) error(src, "Expected until block");

	block->exp = parse_exp(src);

	if(!block->exp) error(src, "Expected expression");

	dlog("[Parsed repeat block]");

	return block;
}

struct Block* parse_do_block(struct FileHandle* src){

	if(!eat_str_token(src, TOKEN_KEYWORD, "do")) return NULL;

	struct Block* block = parse_block(src);

	if(!eat_str_token(src, TOKEN_KEYWORD, "end")) error(src, "Expected end of do block");

	return block;
}

struct Local_Func_Decl* parse_local_func(struct FileHandle* src){

	if(!eat_str_token(src, TOKEN_KEYWORD, "local")) return NULL;
	if(!eat_str_token(src, TOKEN_KEYWORD, "function")) return NULL;

	struct Local_Func_Decl* func = malloc(SSIZE(Local_Func_Decl));

	if(src->current_token.type != TOKEN_IDENT) error(src, "Expected identifier");

	func->name = calloc(strlen(src->current_token.string)+1, sizeof(char));
	strcpy(func->name, src->current_token.string);

	if(!eat_token(src, TOKEN_LPAREN)) error(src, "Expected parameterlist");

	func->params = parse_param_list(src);

	if(!eat_token(src, TOKEN_RPAREN)) error(src, "Expected closing parenthesis");

	func->block = parse_block(src);

	if(!eat_str_token(src, TOKEN_KEYWORD, "end")) error(src, "Expected end of function");

	return func;
}

struct Global_Func_Decl* parse_global_func(struct FileHandle* src){

	// function
	if(!eat_str_token(src, TOKEN_KEYWORD, "function")) return NULL;

	struct Global_Func_Decl* func = malloc(SSIZE(Global_Func_Decl));

	// Name
	func->name = parse_func_name(src);

	// '('
	if(!eat_token(src, TOKEN_LPAREN)) error(src, "Expected parameter list");

	// [paramlist]
	func->params = parse_param_list(src);

	// ')'
	if(!eat_token(src, TOKEN_RPAREN)) error(src, "Expected ending parenthesis");

	// block
	func->block = parse_block(src);

	// end
	if(!eat_str_token(src, TOKEN_KEYWORD, "end")) error(src, "Expected end of function");

	return func;

}


struct If_Block* parse_if_block(struct FileHandle* src){

	// if
	if(!eat_str_token(src, TOKEN_KEYWORD, "if")) return NULL;

	struct If_Block* if_block = calloc(1, SSIZE(If_Block));

	// exp
	if_block->if_exp = parse_exp(src);

	// then
	if(!eat_str_token(src, TOKEN_KEYWORD, "then")) error(src, "Expected 'then' after 'if'");

	// block
	if_block->if_block = parse_block(src);

	// Begin parsing elseif and else blocks

	// elseif
	while(eat_str_token(src, TOKEN_KEYWORD, "elseif")){

		if_block->elif_count++;
		if_block->elif_blocks = realloc(if_block->elif_blocks, SSIZE(If_Block) * if_block->elif_count);

		// Get ptr to newly allocated if block
		struct If_Block* elseif = &if_block->elif_blocks[ if_block->elif_count - 1 ];

		// elseif block can't have sub-else if's connected to this main statement
		elseif->elif_blocks = NULL;
		elseif->elif_count = 0;

		elseif->if_exp = parse_exp(src);

		// then
		if(!eat_str_token(src, TOKEN_KEYWORD, "then")) error(src, "Expected 'then' after 'elseif'");

		// block
		elseif->if_block = parse_block(src);
	}

	// else
	if(eat_str_token(src, TOKEN_KEYWORD, "else")) {
		// block
		if_block->else_block = parse_block(src);
	}

	//end
	if(!eat_str_token(src, TOKEN_KEYWORD, "end")) error(src, "Expected end of if statement");

	return if_block;
}


struct Indexed_For_Block* parse_indexed_for(struct FileHandle* src){

	// for
	if(!eat_str_token(src, TOKEN_KEYWORD, "for")) return NULL;

	// Since we have 2 different versions of for loops
	// we will need to try parsing a variable and an expression
	// Only if those succeed do we know it's an indexed for loop

	// var
	struct Variable_Decl* var = parse_var(src, true);
	if(!var || !eat_token(src, TOKEN_COMMA)) return NULL; // It's a forloop with one iterator: Name

	struct Expression* expr = parse_exp(src);
	if(!expr) return NULL; // This is a namelist, which means an iterator loop
	// Now we know it's an indexed loop

	var->exp = expr; // initial value of variable

	struct Indexed_For_Block* for_block = calloc(1, SSIZE(Indexed_For_Block));

	for_block->index_var = var;
	for_block->limit = parse_exp(src);

	if(!for_block->limit) error(src, "Expected limit in for-loop");

	// optional
	for_block->step = parse_exp(src);

	if(!eat_str_token(src, TOKEN_KEYWORD, "do")) error(src, "Expected 'do' block");

	for_block->block = parse_block(src);

	if(!eat_str_token(src, TOKEN_KEYWORD, "end")) error(src, "Expected 'end' of for loop block");

	return for_block;
}

struct Iter_For_Block* parse_iter_for(struct FileHandle* src){

	if(!eat_str_token(src, TOKEN_KEYWORD, "for")) return NULL;

	struct Name_List* names = parse_name_list(src);

	if(!names) error(src, "Expected identifier");
	if(!eat_str_token(src, TOKEN_KEYWORD, "in")) error(src, "Expected 'in' keyword");

	struct Iter_For_Block* iter_block = calloc(1, SSIZE(Iter_For_Block));

	iter_block->names = names;

	iter_block->explist = parse_exp_list(src);

	if(!eat_str_token(src, TOKEN_KEYWORD, "do")) error(src, "Expected 'do' keyword");

	iter_block->block = parse_block(src);

	if(!eat_str_token(src, TOKEN_KEYWORD, "end")) error(src, "Expected end of for block");

	return iter_block;
}


void print_exp(struct Expression* exp);

void helper_parse_varlist_explist(struct FileHandle* src, struct Var_Decl_List* vars){

	// Assume we've got some variables and the equals sign has been consumed


	struct Expression* tmp = parse_exp(src);
	int index = 0;
	if(tmp) { print_exp(tmp); printf("<-- value of expr\n"); }
	if(vars->decl_count > index){
		vars->decl[0].exp = tmp;
		index++;
	}
	while(eat_token(src, TOKEN_COMMA)) {
		dlog("Found a comma, parsing another expression");
		tmp = parse_exp(src);
		if(tmp) { print_exp(tmp); printf("<-- value of expr\n"); }
		if(vars->decl_count > index){
			vars->decl[index].exp = tmp;
			index++;
		}
		// else, just throw away the expression
	}
	dlog("done parsing expressionlist");
}

struct Statement* parse_stat(struct FileHandle* src) {


	struct Token token = src->current_token;

	if(token.type == TOKEN_END_OF_FILE){
		dlog("Found end of file");
		return NULL;
	}
	dlog("Parsing statement");

	struct Statement* stat = calloc(1, SSIZE(Statement));

	FileMark m = set_mark(src);

	struct Var_Decl_List* varlist = parse_var_list(src);
	if(varlist) {
		stat->type = STAT_VAR_DECL;
		stat->var_list = varlist;

		// Now find an equals
		if(eat_token(src, TOKEN_EQUAL)){
			dlog("Parsed varlist, now expression list for varlist");
			// Start parsing expressions and put them into variable expressions
			helper_parse_varlist_explist(src, varlist);

			dlog("Done parsing varlist");
			free_mark(&m);
			return stat;
		}
	}
	goto_mark(src, m);

dlog("[stat] 1");

	struct Function_Call* func_call = parse_func_call(src, true, false);
	if(func_call) {
		dlog("[stat] Parsed func call");
		stat->type = STAT_FUNC_CALL;
		stat->func_call = func_call;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

dlog("[stat] 2");

	struct Block* do_block = parse_do_block(src);
	if(do_block){
		dlog("[stat] Parsed do block");
		stat->type = STAT_DO_BLOCK;
		stat->do_block = do_block;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

dlog("[stat] 3");

	struct While_Block* while_block = parse_while_block(src);
	if(while_block){
		dlog("[stat] Parsed while block");
		stat->type = STAT_WHILE_BLOCK;
		stat->while_repeat_block = while_block;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

//dlog("[stat] 4");

	struct While_Block* repeat_block = parse_repeat_block(src);
	if(repeat_block){
		dlog("[stat] Parsed repeat block");
		stat->type = STAT_REPEAT_BLOCK;
		stat->while_repeat_block = repeat_block;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

//dlog("[stat] 5");

	struct If_Block* if_block = parse_if_block(src);
	if(if_block){
		dlog("[stat] Parsed if block");
		stat->type = STAT_IF_BLOCK;
		stat->if_block = if_block;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Indexed_For_Block* indexed_for = parse_indexed_for(src);
	if(indexed_for){
		dlog("[stat] Parsed indexed for");
		stat->type = STAT_FOR_INDEXED;
		stat->indexed_for = indexed_for;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Iter_For_Block* iter_for = parse_iter_for(src);
	if(iter_for){
		dlog("[stat] Parsed iter for");
		stat->type = STAT_FOR_IN;
		stat->iter_for_block = iter_for;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Global_Func_Decl* global_func = parse_global_func(src);
	if(global_func){
		dlog("[stat] Parsed glob func");
		stat->type = STAT_GLOBAL_FUNC_DECL;
		stat->global_func = global_func;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Local_Func_Decl* local_func = parse_local_func(src);
	if(local_func){
		dlog("[stat] Parsed local func");
		stat->type = STAT_LOCAL_FUNC_DECL;
		stat->local_func = local_func;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	if(eat_str_token(src, TOKEN_KEYWORD, "return")){
		dlog("[stat] Parsed return");
		stat->type = STAT_RETURN;
		stat->exprlist = parse_exp_list(src);
		free_mark(&m);
		return stat;
	}
	else if(eat_str_token(src, TOKEN_KEYWORD, "break")){
		dlog("[stat] Parsed break");
		stat->type = STAT_BREAK;
		free_mark(&m);
		return stat;
	}
	else if(eat_str_token(src, TOKEN_KEYWORD, "local")){

		struct Var_Decl_List* varlist = parse_var_list(src);
		if(varlist && eat_token(src, TOKEN_EQUAL)) {
			dlog("[stat] Parsed local var decl");
			helper_parse_varlist_explist(src, varlist);
			stat->type = STAT_LOCAL_VAR_DECL;
			stat->var_list = varlist;
			free_mark(&m);
			return stat;
		}
		goto_mark(src, m);
	}

	dlog("Could not parse any statement");
	dlog(tokenizer_token_str(src->current_token));
	if(src->current_token.string != NULL)
		dlog(src->current_token.string);

	free_mark(&m);

	error(src, "Current token couldn't be parsed");
	return NULL;
}

/*
 * Block parsing is trivial, just parse statements until no more can be parsed
 *
*/
struct Block* parse_block(struct FileHandle* src){

	struct Block* b = calloc(1, SSIZE(Block));

	b->stat_count = 0;

	struct Statement* tmpStat = parse_stat(src);

	struct Statement* outArr = NULL;
	int out_len = 0;

	// Parsing statements until no more statements can be parsed
	while(tmpStat != NULL) {
		dlog(tokenizer_token_str(src->current_token));
		out_len += 1;
		outArr = realloc(outArr, SSIZE(Statement) * out_len);
		memcpy(&outArr[out_len-1], tmpStat, SSIZE(Statement));

		free(tmpStat);
		tmpStat = parse_stat(src);
	}

	// Reached end, check last statement if it's 'laststat'
	// aka a return statement or break

	dlog("[block] done parsing statements, checking last statement for return/break");
	b->laststat = NULL;

	if(out_len > 0) {
		struct Statement* last = &outArr[out_len-1];

		// Move from statement list to laststat
		// We need to reallocate the whole array and the laststat var
		if(last->type == STAT_RETURN || last->type == STAT_BREAK) {
			dlog("[block] found return/break");
			b->laststat = malloc(SSIZE(Statement));
			memcpy(b->laststat, &outArr[out_len-1], SSIZE(Statement));
			out_len--;
			outArr = realloc(outArr, SSIZE(Statement) * out_len);
		}
	}


	b->stat = outArr;
	b->stat_count = out_len;
	return b;
}


/*
 * OK
 *
 * Any valid prefix_exp MUST begin with either an identifier or a LPAREN
*/
struct Prefix_Exp* parse_prefix_exp(struct FileHandle* src, bool nullable) {

	if(src->current_token.type == TOKEN_END_OF_FILE) return NULL;
	if(src->current_token.type != TOKEN_IDENT &&
		src->current_token.type != TOKEN_LPAREN) return NULL;

	struct Prefix_Exp* pre = calloc(1, SSIZE(Prefix_Exp));

	FileMark m = set_mark(src);

	pre->var_decl = parse_var(src, true);
	if(pre->var_decl) {
		dlog("[prefix_exp] Parsed variable");
		pre->type = PRE_VAR;

		// This is an edge case where we need to try and parse a function call
		struct Function_Call* func = parse_func_call(src, true, true);
		if(func){
			func->prefix = pre;
			free_mark(&m);
			// The prefix exp var gets consumed by the func call object
			// So we need to make a new one
			struct Prefix_Exp* pre2 = calloc(1, SSIZE(Prefix_Exp));
			pre2->func_call = func;
			pre2->type = PRE_FUNC;
			dlog("[prefix] Parsed variable which turned out to be a func call");
			return pre2;
		}

		free_mark(&m);
		return pre;
	}
	goto_mark(src, m); // Put back any characters that might have been consumed

	pre->func_call = parse_func_call(src, true, false);
	if(pre->func_call) {
		dlog("[prefix] Parsed function call");
		pre->type = PRE_FUNC;
		free_mark(&m);
		return pre;
	}
	goto_mark(src,m);



	if(eat_token(src, TOKEN_LPAREN)){
		pre->expr = parse_exp(src);
		if(pre->expr && eat_token(src, TOKEN_RPAREN)){
			dlog("[prefix] Expression within parens parsed: '(' exp ')' ");
			pre->type = PRE_EXPR;
			free_mark(&m);
			return pre;
		}
		if(!nullable) error(src, "Expected expression and closing parenthesis");
		dlog("[prefix] failed parsing expression in parenthesis, left paren found though");
		goto_mark(src, m);
		free_mark(&m);
		return NULL;
	}
	//  nothing parsed :o
	dlog("No result for parsing prefix");
	if(!nullable) error(src, "Expected prefix expression");
	goto_mark(src, m);
	free_mark(&m);
	return NULL;
}

/*
 * OK
 *
*/
struct Variable_Decl* parse_var(struct FileHandle* src, bool nullable) {

	if(src->current_token.type == TOKEN_END_OF_FILE) return NULL;

	struct Variable_Decl* var = calloc(1, SSIZE(Variable_Decl));
	struct Prefix_Exp* pre = NULL;
	bool var_correct_ending_symbol = false;

	FileMark m = set_mark(src);

	struct Token token = src->current_token;


	// identifier
		//if '.' follows, parse another var, this is a prefix
		// if '[' follows, parse exp and ']'
			// if '(' then we got a func call
			// if '.' then parse this as prefix, parse a var, var cannot be null
			// if '[' then this->prefix parse var
	// LPAREN - it's a prefix exp
		// parse exp and RPAREN followed by '.' | '['



	// This is a prefix parsed in var to prevent recursion loops
	// Parsing '(' exp ')'
	// Still parsing the same var decl, only this exp is prefix to it
	if(eat_token(src, TOKEN_LPAREN)){
		pre = calloc(1, SSIZE(Prefix_Exp));
		pre->type = PRE_EXPR;
		pre->expr = parse_exp(src);
		if(!eat_token(src, TOKEN_RPAREN)){
			free_mark(&m);
			if(nullable) return NULL;
			error(src, "Expected end parenthesis");
		}
		var->prefix = pre;
		var_correct_ending_symbol = false;
	}
	else if(token.type == TOKEN_IDENT){
		// Can't have prefix expr and an ident following
		// So we choose either one
		var->name = calloc(strlen(token.string)+1, sizeof(char));
		strcpy(var->name,token.string);
		eat_token(src, TOKEN_IDENT);
		var_correct_ending_symbol = true;
	} else {
		free_mark(&m);
		if(nullable) return NULL;
		error(src, "Expected identifier or '(' expression ')'");
	}

	token = src->current_token;
	// Now we have either an prefix expression or an identifier parsed

	// Either var->pre or var->name is valid

	while(token.type == TOKEN_LPAREN ||
		token.type == TOKEN_LBRACKET ||
		token.type == TOKEN_STRING ||
		token.type == TOKEN_COLON){
		// This be function call :3

		struct Function_Call* func = calloc(1, SSIZE(Function_Call));
		if(!pre)
			(pre = calloc(1, SSIZE(Prefix_Exp)))->var_decl = var;

		pre->type = PRE_VAR;
		var = NULL;
		func->prefix = pre; pre = NULL; // give our prefix to func

		if(eat_token(src, TOKEN_COLON)) {
			token = src->current_token;
			if(token.type != TOKEN_IDENT) {
				free_mark(&m);
				if(nullable) return NULL;
				error(src, "Expected identifier following :");
			}
			func->name = calloc(strlen(token.string)+1, sizeof(char));
			strcpy(func->name, token.string);
		}

		// Now parse args
		func->args = parse_args(src); // Parse args eats up parenthesis,table or string

		// This function call must now be parsed as a prefix
		(pre = calloc(1, SSIZE(Prefix_Exp)))->type = PRE_FUNC;
		pre->func_call = func;

		var = calloc(1, SSIZE(Variable_Decl));
		var->prefix = pre; // it's a prefix to our variable
		// Our function call with any prefixes are stored in pre
		token = src->current_token;
		// This can be recursed since function call can return a function
		// Hence we keep trying to parse another function call
		var_correct_ending_symbol = false; // Can't end on a prefix
	}


	// From this point on var can be an empty declaration with a prefix
	// , a variable with name or a variable with an expression

	// Parsing prefixexp '[' exp ']'
	if(eat_token(src, TOKEN_LSQBRACKET)){

		if(var->exp) {
			// Can't have a name or a prefix from earlier
			if(var->prefix || var->name)
				error(src, "Unexpected compiler state, variable wasn't expected to have prefix or name at this stage");
			// We got an expression here already, this means it's a prefix now
			(pre = calloc(1, SSIZE(Prefix_Exp)))->type = PRE_EXPR;
			pre->expr = var->exp; // var->exp gets overwritten when we parse exp
			var->prefix = pre;
		}
		var->exp = parse_exp(src);
		if(!eat_token(src, TOKEN_RSQBRACKET)) {
			free_mark(&m);
			if(nullable){return NULL;}
			error(src, "Expected end square bracket");
		}
		var_correct_ending_symbol = true;
	}


	// We can have an empty declaration with a prefix,
	// a variable with a name, a variable with an expression
	// along with: pre + exp, name + exp
	if(eat_token(src, TOKEN_DOT)){
		var_correct_ending_symbol = false;

		// Recurse
		// This means our var is a prefix
		(pre = calloc(1, SSIZE(Prefix_Exp)))->type = PRE_VAR;

		if(var->exp && !var->name && !var->prefix){
			pre->type = PRE_EXPR;
			pre->expr = var->exp;
			free(var);
		}
		else if( // If we have expression + either prefix or name (or name alone) then it's a var
			(var->exp && ( ( var->prefix && !var->name ) || ( !var->prefix && var->name ))) ||
			var->name)
		{
			pre->var_decl = var;
			var = NULL;
		}
		else
			error(src, "Unexpected compiler state, variable is undefined");

		var = parse_var(src, nullable);

		if(!var) {
			free_mark(&m);
			if(nullable) return NULL;
			error(src, "Variable must be an identifier");
		}

		// By the use of pre-prefix we attach this prefix to the next one following the '.' char
		if(var->prefix)
			var->pre_prefix = pre;
		else
			var->prefix = pre;

	}

	if(!var_correct_ending_symbol){
		// We've ended on a func call or prefix which isn't valid
		free_mark(&m);
		if(nullable) return NULL;
		error(src, "Expected variable, got invalid symbol");
	}

	return var;

	// We never assign to var->exp so this wont run ever
	// Parsing var '[' exp ']'
	/*
	if(var->exp && eat_token(src, TOKEN_LSQBRACKET)){
		(pre = calloc(1, SSIZE(Prefix_Exp))->type = PRE_VAR;
		if(!var) { free_mark(&m);
			if(nullable) return NULL;
			error(src, "Expected variable declaration");
		}
		pre->var_decl = var;
		(var = calloc(1, SSIZE(Variable_Decl))->prefix = pre;
		var->exp = parse_exp(src);
		if(!eat_token(src, TOKEN_RSQBRACKET)) {
			free_mark(&m);
			if(nullable) return NULL;
			error(src, "Expected end square bracket");
		}
	}*/



	if(token.type == TOKEN_IDENT) {
		var->name = calloc(strlen(token.string)+1,sizeof(char));
		strcpy(var->name,token.string);
		eat_token(src, TOKEN_IDENT);
		token = src->current_token;
		if(token.type == TOKEN_LSQBRACKET) {
			// A-HAA indexed variable! gotcha
			// our earlier identifier is part of prefixexp
			struct Prefix_Exp* pre = calloc(1, SSIZE(Prefix_Exp));
			pre->type = PRE_VAR;
			pre->var_decl = var;
			eat_token(src, TOKEN_LSQBRACKET);
			// Since we gave away our var decl, make a new one
			var = calloc(1, SSIZE(Variable_Decl));
			var->prefix = pre;
			var->exp = parse_exp(src);
			if(!eat_token(src, TOKEN_RSQBRACKET)) error(src, "Expected ending square bracket to close indexer operator");
			free(&m);
			return var; // Parsed Name[exp]
		}
		else if(eat_token(src, TOKEN_DOT)) {
			// A wild identifier appears...maybe
			token = src->current_token;
			if(token.type != TOKEN_IDENT) error(src, "Expected identifier");
		}
	}









	if(src->current_token.type == TOKEN_IDENT) {
		var->name = calloc(strlen(src->current_token.string)+1, sizeof(char));
		strcpy(var->name, src->current_token.string);
		printf("[var] Parsed identifier: %s\n",src->current_token.string);
		eat_token(src, TOKEN_IDENT);
	}
	else {
		var->prefix = parse_prefix_exp(src, true);
		if(!var->prefix) {
			goto_mark(src, m);
			free_mark(&m);
			if(nullable) return NULL;
			error(src, "Expected prefix expression");
		}
		if(eat_token(src, TOKEN_DOT)) {
			if(src->current_token.type != TOKEN_IDENT) {
				goto_mark(src, m);
				free_mark(&m);
				if(nullable) return NULL;
				error(src, "Expected identifier");
			}
			dlog("[var] Parsed prefix_exp followed by '.' and an identifier");
			var->name = calloc(strlen(src->current_token.string)+1, sizeof(char));
			strcpy(var->name, src->current_token.string);
			eat_token(src, TOKEN_IDENT);
		}
		else if(eat_token(src, TOKEN_LSQBRACKET)) {
			var->exp = parse_exp(src);
			if(!var->exp) {
				goto_mark(src, m);
				free_mark(&m);
				if(nullable) return NULL;
				error(src, "Expected expression");
			}
			if(!eat_token(src, TOKEN_RSQBRACKET)){
				goto_mark(src, m);
				free_mark(&m);
				if(nullable) return NULL;
				error(src, "Expected closing square bracket");
			}
			dlog("[var] Parsed indexed variable '[' exp ']'");
		}
		else {
			goto_mark(src, m);
			free_mark(&m);
			if(nullable) return NULL;
			error(src, "Unexpected symbol");
		}
	}
	free_mark(&m);
	return var;
}


// varlist
struct Var_Decl_List* parse_var_list(struct FileHandle* src) {

	FileMark m = set_mark(src);

	struct Variable_Decl* var = parse_var(src, true);

	if(!var) {
		goto_mark(src, m);
		free_mark(&m);
		return NULL;
	}

	dlog("[varlist] Parsed first var");

	// Since functioncall also can begin with a variable
	// we cannot know for sure if it's a variable list or func call
	// let @stat parsing handle the error if func call cannot be parsed

	struct Var_Decl_List* varlist = calloc(1, SSIZE(Var_Decl_List));
	varlist->decl_count = 1;

	varlist->decl = malloc(SSIZE(Variable_Decl));
	memcpy(&varlist->decl[0], var, SSIZE(Variable_Decl));

	while(eat_token(src, TOKEN_COMMA)) {
		// By this point we know it is a variable list
		// thus if we cannot parse another var, error
		var = parse_var(src, false);

		dlog("[varlist] Parsed another var");
		// Var is guaranteed to be not null
		varlist->decl_count++;
		varlist->decl = realloc(varlist->decl, SSIZE(Variable_Decl) * varlist->decl_count);
		memcpy(&varlist->decl[varlist->decl_count-1], var, SSIZE(Variable_Decl));
	}

	free_mark(&m);
	return varlist;
}


/*
 * OK
*/
struct Expression_List* parse_exp_list(struct FileHandle* src) {

	struct Expression_List* exprs = NULL;
	int expr_c = 0;

	struct Expression* tmp = NULL;
	tmp = parse_exp(src);

	while(tmp != NULL) {
		if(!exprs) exprs = malloc(SSIZE(Expression_List));

		expr_c++;
		exprs->exprs = realloc(exprs->exprs, SSIZE(Expression) * expr_c);
		memcpy(&exprs->exprs[expr_c-1], tmp, SSIZE(Expression));

		printf("[DEBUG] [explist] Parsed %d expression\n", expr_c);

		if(!eat_token(src, TOKEN_COMMA)) break; // continue only if we found a comma

		printf("[DEBUG] [explist] Found a comma, parsing another exp\n");

		tmp = parse_exp(src);

		if(!tmp) error(src, "Expected expression"); // List has to end on an expression
	}


	if(expr_c == 0) {
		error(src, "Expected at least one expression");
	}

	exprs->exp_count = expr_c;
	return exprs;
}


/*
 * OK
*/
struct Args* parse_args(struct FileHandle* src) {

	struct Args* args = calloc(1, SSIZE(Args));

	if(eat_token(src, TOKEN_LPAREN)) {
		dlog("[args parsing explist]");
		args->explist = parse_exp_list(src);
		if(!eat_token(src, TOKEN_RPAREN)) error(src, "Expected closing parenthesis");
	}
	else if(src->current_token.type == TOKEN_STRING)
	{
		dlog("[args parsing string]");
		args->string = calloc(strlen(src->current_token.string)+1,sizeof(char));
		strcpy(args->string, src->current_token.string);
	}
	else
	{
		dlog("[args parsing table constructor]");
		// Table constructor is the last option here
		args->table = parse_table_cons(src);
		// We can have no arguments at all
		if(!args->table) { free(args); return NULL; }
	}

	return args;
}

/*
 * OK
*/
struct Function_Call* parse_func_call(struct FileHandle* src, bool nullable, bool skip_prefix) {

	dlog("[Func_Call] parsing prefix");

	FileMark m = set_mark(src);

	struct Prefix_Exp* pre = NULL;

	if(!skip_prefix) {
		pre = parse_prefix_exp(src, true);

		if(!pre) {
			goto_mark(src, m);
			free_mark(&m);
			if(nullable) return NULL;
			error(src, "Expected prefix expression");
		}
	}

	struct Function_Call* func = calloc(1, SSIZE(Function_Call));
	if(!skip_prefix)
		func->prefix = pre;

	if(eat_token(src, TOKEN_COLON)){
		if(src->current_token.type != TOKEN_IDENT) {
			goto_mark(src, m);
			free_mark(&m);
			free(func);
			if(nullable) return NULL;
			error(src, "Expected function name");
		}
		func->name = calloc(strlen(src->current_token.string)+1, sizeof(char));
		strcpy(func->name, src->current_token.string);
		eat_token(src, TOKEN_IDENT);
	}

	dlog("[Func_Call] parsing args");

	func->args = parse_args(src);
	free_mark(&m);
	return func;
}


struct Block* parse_AST(struct FileHandle* src){
	return parse_block(src);
}

//
//	Pretty print section
//

void print_func_call(struct Function_Call* call);
void print_prefix_exp(struct Prefix_Exp* exp);

void print_exp(struct Expression* exp){
	if(!exp) return;
	switch(exp->type){
	case EXP_NUMBER: printf("%f",exp->number); break;
	case EXP_STRING: printf("%s",exp->string); break;
	default: printf("Unknown/unimplemented expression type");
	}
}

void print_var_list(struct Var_Decl_List* vars){
	if(!vars) return;

	printf("[varlist %i]\n", vars->decl_count);
	for(int i = 0; i < vars->decl_count; i++) {
		printf("\t[var: '%s' exp:'", vars->decl[i].name);
		print_exp(vars->decl[i].exp);
		printf("' ]\n");
	}
	printf("\n");
}


void print_var(struct Variable_Decl* decl){
	if(!decl) return;
	printf(" [var: ");
	if(decl->name) printf("%s",decl->name);
	print_prefix_exp(decl->prefix);
	print_exp(decl->exp);
	printf("]");
}

void print_prefix_exp(struct Prefix_Exp* exp){
	if(!exp) return;
	if(exp->type == PRE_EXPR) print_exp(exp->expr);
	else if(exp->type == PRE_FUNC) print_func_call(exp->func_call);
	else if(exp->type == PRE_VAR) print_var(exp->var_decl);
}

void print_exp_list(struct Expression_List* explist){
	if(!explist)return;
	for(int i =0; i < explist->exp_count; i++) {
		print_exp(&explist->exprs[i]);
		if(i != explist->exp_count -1) printf(", ");
	}
}

void print_field(struct Field* f){
	if(!f) return;
	print_exp(f->left);
	printf(" %s ", f->name);
	print_exp(f->right);
}

void print_table(struct Table* t){
	if(!t) return;
	for(int i=0; i < t->field_count; i++){
		print_field(&t->fields[i]);
		if(i != t->field_count-1) printf(", ");
	}
}

void print_args(struct Args* args){
	if(!args) return;
	print_exp_list(args->explist);
	print_table(args->table);
	if(args->string) printf("%s",args->string);
}

void print_func_call(struct Function_Call* call){
	printf("Func call to ");
	print_prefix_exp(call->prefix);
	printf("( args: ");
	print_args(call->args);
	printf(" )\n");
}

void print_stat(struct Statement* stat){
	if(!stat) return;
	printf("[Statement]\n");
	switch(stat->type){
	case STAT_LOCAL_VAR_DECL:
	printf("local ");
	case STAT_VAR_DECL: print_var_list(stat->var_list); break;
	case STAT_FUNC_CALL: print_func_call(stat->func_call); break;
	default: printf("Unknown statement type\n");
	}
	printf("[END]\n");
}

void print_AST(struct Block* blk) {
	if(!blk) return;
	printf("[Block]\n");
	for(int i = 0; i < blk->stat_count; i++)
		print_stat(&blk->stat[i]);

	print_stat(blk->laststat);
}
#endif
