#include "parser.h"
#include "common.h"

#include <stdlib.h>
#include <string.h>

typedef struct {
	struct FileHandle handlecpy;
	char* token_str;
} FileMark;

FileMark set_mark(struct FileHandle* src){
	FileMark mark;
	memcpy(&mark.handlecpy, src, sizeof(struct FileHandle));
	mark.token_str = null;

	if(mark.handlecpy.current_token.type != TOKEN_IDENT &&
		mark.handlecpy.current_token.type != TOKEN_STRING &&
		mark.handlecpy.current_token.type != TOKEN_KEYWORD) return mark;

	if(mark.handlecpy.current_token.string == null) return mark;

	mark.token_str = (char*)calloc(strlen(src->current_token.string)+1,sizeof(char));
	strcpy(mark.token_str, src->current_token.string);
	return mark;
}

void free_mark(FileMark* mark){
	if(!mark) return;
	free(mark->token_str);
	mark->token_str = null;
}

void goto_mark(struct FileHandle* src, FileMark mark){

	memcpy(src, &mark.handlecpy, sizeof(struct FileHandle));

	//src->cursor_pos = mark.cursor_pos;
	//src->line_nr = mark.linenr;
	//src->current_token = mark.token;
	if(mark.handlecpy.current_token.type != TOKEN_IDENT &&
		mark.handlecpy.current_token.type != TOKEN_STRING &&
		mark.handlecpy.current_token.type != TOKEN_KEYWORD) return;
	if(mark.token_str == null) return;
	src->current_token.string = (char*)calloc(strlen(mark.token_str)+1, sizeof(char));
	strcpy(src->current_token.string, mark.token_str);
}


void debug_print_state(struct FileHandle* src){
	printf("-------------------------------\n");
	printf("Current token; %s",tokenizer_token_str(src->current_token));
	enum TokenType t = src->current_token.type;
	if(t == TOKEN_STRING || t == TOKEN_IDENT ||
		t == TOKEN_KEYWORD)
		printf("with string %s", src->current_token.string);

	printf("\n");
	printf("-------------------------------\n");
}


bool is_token(struct FileHandle* src, enum TokenType t){
	return src->current_token.type == t;
}

bool eat_token(struct FileHandle* src, enum TokenType t){
	if(!is_token(src, t)) return false;
	tokenizer_move_next(src);
	return true;
}

void assert_eat_token(struct FileHandle* src, enum TokenType t){
	if(!is_token(src, t)) { error(src, "Unexpected symbol"); }
	eat_token(src, t);
}


bool eat_str_token(struct FileHandle* src, const char* str){
	if(!is_token(src, TOKEN_STRING)) return false;
	if(strcmp(src->current_token.string, str) != 0) return false;
	tokenizer_move_next(src);
	return true;
}

bool is_keyword(struct FileHandle* src, const char* str){
	if(!is_token(src, TOKEN_KEYWORD)) return false;
	if(strcmp(src->current_token.string, str) != 0) return false;
	return true;
}

bool eat_keyword(struct FileHandle* src, const char* str){
	if(!is_token(src, TOKEN_KEYWORD)) return false;
	if(strcmp(src->current_token.string, str) != 0) return false;
	tokenizer_move_next(src);
	return true;
}


//
// Forward declarations
//
struct Expression* parse_exp(struct FileHandle* src);
struct Expression* parse_exp_atomic(struct FileHandle* src);
struct Variable_Decl* parse_ident(struct FileHandle* src);
struct Variable_Decl* parse_var(struct FileHandle* src, bool nullable, struct Function_Call** func_out);
struct Block* parse_block(struct FileHandle* src);
struct Statement* parse_stat(struct FileHandle* src);
struct Var_Decl_List* parse_var_list(struct FileHandle* src);
struct Prefix_Exp* parse_prefix_exp(struct FileHandle* src);
struct Args* parse_args(struct FileHandle* src);
struct Expression_List* parse_exp_list(struct FileHandle* src);
struct Table* parse_table_cons(struct FileHandle* src);
struct Block* parse_do_block(struct FileHandle* src);
struct While_Block* parse_repeat_block(struct FileHandle* src);
struct Local_Func_Decl* parse_local_func(struct FileHandle* src);
//
//
//

struct Prefix_Exp* parse_prefix_exp(struct FileHandle* src){

	struct Function_Call* func = null;
	struct Variable_Decl* var = parse_var(src, true, &func);
	if(var){
		struct Prefix_Exp* pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
		pre->type = PRE_VAR;
		pre->var_decl = var;
		return pre;
	}
	if(func){
		struct Prefix_Exp* pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
		pre->type = PRE_FUNC;
		pre->func_call = func;
		return pre;
	}
	if(eat_token(src, TOKEN_LPAREN)){
		struct Prefix_Exp* pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
		pre->type = PRE_EXPR;
		pre->exp = parse_exp(src);
		if(!eat_token(src, TOKEN_RPAREN)) error(src, "Expected closing paren");
	}
	return null;
}

struct Args* parse_args(struct FileHandle* src) {
	if(!is_token(src, TOKEN_LPAREN) &&
		!is_token(src, TOKEN_LBRACKET) &&
		!is_token(src, TOKEN_STRING)) return null;

	struct Args* args = (struct Args*)calloc(1, SSIZE(Args));

	if(is_token(src, TOKEN_LPAREN)){
		eat_token(src, TOKEN_LPAREN);
		args->explist = parse_exp_list(src);
		if(!eat_token(src, TOKEN_RPAREN)) return null;
	}
	else if(is_token(src, TOKEN_LBRACKET)){
		args->table = parse_table_cons(src);
	}
	else if(is_token(src, TOKEN_STRING)){
		printf("string as args\n");
		args->string = (char*)calloc(strlen(src->current_token.string)+1,sizeof(char));
		strcpy(args->string, src->current_token.string);
		eat_token(src, TOKEN_STRING);
	}

	return args;
}

struct Var_Decl_List* parse_var_list(struct FileHandle* src) {

	FileMark m = set_mark(src);

	struct Variable_Decl* var = parse_var(src, true, null);

	if(!var){
		goto_mark(src,m);
		free_mark(&m);
		return null;
	}

	printf("[varlist] Parsed a variable");
	if(var->name) printf(" '%s'", var->name);
	printf(", trying to find more...\n");

	struct Var_Decl_List* varlist = (struct Var_Decl_List*)calloc(1, SSIZE(Var_Decl_List));
	varlist->decl_count = 1;

	varlist->decl = (struct Variable_Decl*)malloc(SSIZE(Variable_Decl));
	memcpy(&varlist->decl[0], var, SSIZE(Variable_Decl));

	while(eat_token(src, TOKEN_COMMA)){
		var = parse_var(src, false, null);
		printf("[varlist] Parsed another variable");
		if(var->name) printf(" '%s'", var->name);
		printf("\n");
		varlist->decl_count++;
		varlist->decl = (struct Variable_Decl*)realloc(varlist->decl, SSIZE(Variable_Decl) * varlist->decl_count);
		memcpy(&varlist->decl[varlist->decl_count-1], var, SSIZE(Variable_Decl));
	}

	printf("[varlist] end\n");

	free_mark(&m);
	return varlist;
}


struct Expression_List* parse_exp_list(struct FileHandle* src){

	struct Expression_List* explist = null;
	int expc = 0;

	struct Expression* exp = null;
	exp = parse_exp(src);

	while(exp){
		if(!explist) explist = (struct Expression_List*)calloc(1, SSIZE(Expression_List));

		expc++;
		explist->exprs = (struct Expression*)realloc(explist->exprs, SSIZE(Expression) * expc);
		memcpy(&explist->exprs[expc -1], exp, SSIZE(Expression));

		if(!eat_token(src, TOKEN_COMMA)) break;

		exp = parse_exp(src);

		if(!exp) error(src, "Expected expression");
	}

	if(expc == 0) error(src, "Expected at least one expression");

	explist->exp_count = expc;
	return explist;

}


void helper_parse_varlist_explist(struct FileHandle* src, struct Var_Decl_List* vars){
	struct Expression* tmp = parse_exp(src);
	int index = 0;
	if(!tmp) error(src, "Expected at least one expression");
	struct Variable_Decl* decl = &vars->decl[0];
	if(vars->decl_count > 0) {
		while(decl->after_dot_var) decl = decl->after_dot_var;
		decl->value = tmp;
	}
	index++;
	decl = null;
	while(eat_token(src, TOKEN_COMMA)){
		tmp = parse_exp(src);
		if(vars->decl_count > index){
			decl = &vars->decl[index];
			while(decl->after_dot_var) decl = decl->after_dot_var;
			decl->value = tmp;
		}
		index++;
	}
}


struct Name_List* parse_name_list(struct FileHandle* src){
	if(!is_token(src, TOKEN_IDENT)) return null;
	// Important to init to zero since we use realloc for initial allocation
	struct Name_List* list = (struct Name_List*)calloc(1, SSIZE(Name_List));
	list->names = null;//malloc(sizeof(char*));
	int name_count = 0;

	while(is_token(src, TOKEN_IDENT)){
		name_count++;
		list->names = (char**)realloc(list->names, sizeof(char*) * name_count);
		list->names[name_count-1] = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
		strcpy(list->names[name_count-1], src->current_token.string);

		eat_token(src, TOKEN_IDENT);
		if(!eat_token(src, TOKEN_COMMA)) break;
	}
	list->name_count = name_count;
	return list;
}


struct Parameter_List* parse_param_list(struct FileHandle* src){
	struct Name_List* names = parse_name_list(src);

	struct Parameter_List* params = (struct Parameter_List*)malloc(SSIZE(Parameter_List));
	params->names = names;

	params->varargs_end = false;
	if(is_token(src, TOKEN_TDOT))
		params->varargs_end = true;

	if(!eat_token(src, TOKEN_TDOT) && !names) return null;
	return params;
}


struct Func_Name* parse_func_name(struct FileHandle* src){
	if(!is_token(src, TOKEN_IDENT)) error(src, "Expected function name");

	char** names = (char**)calloc(1, sizeof(char*));
	int name_c = 1;
	names[name_c-1] = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
	strcpy(names[name_c-1], src->current_token.string);
	eat_token(src, TOKEN_IDENT);

	struct Func_Name* funcn = (struct Func_Name*)calloc(1, SSIZE(Func_Name));
	while(1){
		if(eat_token(src, TOKEN_DOT)){
			if(!is_token(src, TOKEN_IDENT)) error(src, "Expected function name");
			name_c++;
			names = (char**)realloc(names, name_c * sizeof(char*));
			names[name_c-1] = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
			strcpy(names[name_c-1], src->current_token.string);
		}
		else
		{
			if(!eat_token(src, TOKEN_COLON)) break;
			funcn->lastname = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
			strcpy(funcn->lastname, src->current_token.string);
			break;
		}
	}

	funcn->names = names;
	funcn->name_c = name_c;
	return funcn;
}


struct Global_Func_Decl* parse_global_func(struct FileHandle* src){

	if(!eat_keyword(src, "function")) return null;

	struct Global_Func_Decl* func = (struct Global_Func_Decl*)malloc(SSIZE(Global_Func_Decl));
	func->name = parse_func_name(src);

	if(!eat_token(src, TOKEN_LPAREN)) error(src, "Expected parameter list");

	func->params = parse_param_list(src);

	if(!eat_token(src, TOKEN_RPAREN)) error(src, "Expected ending parenthesis");

	func->block = parse_block(src);


	//printf("after func body: %s, %s\n",tokenizer_token_str(src->current_token), src->current_token.string);
	if(!eat_keyword(src, "end")) error(src, "Expected end of function");

	return func;
}

struct If_Block* parse_if_block(struct FileHandle* src){
	if(!eat_keyword(src, "if")) return null;
	struct If_Block* block = (struct If_Block*)malloc(SSIZE(If_Block));
	block->if_exp = parse_exp(src);
	if(!eat_keyword(src, "then")) error(src, "Expected 'then'");
	block->if_block = parse_block(src);
	block->elif_count = 0;
	block->elif_blocks = null;
	while(eat_keyword(src, "elseif")){
		block->elif_count++;
		block->elif_blocks = (struct If_Block*)realloc(block->elif_blocks, SSIZE(If_Block) * block->elif_count);
		struct If_Block* elseif = &block->elif_blocks[ block->elif_count -1 ];
		elseif->elif_blocks = null; elseif->elif_count = 0;
		elseif->if_exp = parse_exp(src);
		if(!eat_keyword(src, "then")) error(src, "Expected 'then'");
		elseif->if_block = parse_block(src);
	}
	if(eat_keyword(src, "else")){
		block->else_block = parse_block(src);
	}
	if(!eat_keyword(src, "end")) error(src, "Expected end of if-statement");
	return block;
}

struct Indexed_For_Block* parse_indexed_for(struct FileHandle* src){
	if(!eat_keyword(src, "for")) return null;
	if(!is_token(src, TOKEN_IDENT)) return null;

	// Name
	struct Variable_Decl* var = (struct Variable_Decl*)calloc(1, SSIZE(Variable_Decl));
	var->name = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
	strcpy(var->name, src->current_token.string);

	if(!eat_token(src, TOKEN_EQUAL)) error(src, "Expected iterator start value");
	var->value = parse_exp(src);
	if(!var->value) error(src, "Expected iterator start value");
	if(!eat_token(src, TOKEN_COMMA)) error(src, "Expected iterator limit value");
	struct Indexed_For_Block* for_block = (struct Indexed_For_Block*)calloc(1, SSIZE(Indexed_For_Block));
	for_block->index_var = var;
	for_block->limit = parse_exp(src);
	if(eat_token(src, TOKEN_COMMA)){
		for_block->step = parse_exp(src);
	}
	if(!eat_keyword(src, "do")) error(src, "Expected do-block");
	for_block->block = parse_block(src);
	if(!eat_keyword(src, "end")) error(src, "Expected end of do-block");
	return for_block;
}

struct Iter_For_Block* parse_iter_for(struct FileHandle* src){
	if(!eat_keyword(src, "for")) return null;
	struct Name_List* names = parse_name_list(src);
	if(!names) return null;
	if(!eat_keyword(src, "in")) error(src, "Expected 'in' followed by a list or object");
	struct Iter_For_Block* iter_block = (struct Iter_For_Block*)calloc(1, SSIZE(Iter_For_Block));
	iter_block->names = names;
	iter_block->explist = parse_exp_list(src);
	if(!eat_keyword(src, "do")) error(src, "Expected do-block");
	iter_block->block = parse_block(src);
	if(!eat_keyword(src, "end")) error(src, "Expected end of do-block");
	return iter_block;
}


struct Statement* parse_stat(struct FileHandle* src){
	if(is_token(src, TOKEN_END_OF_FILE)) return null;
	if(is_keyword(src, "end") || is_keyword(src, "else") || is_keyword(src, "elseif")) return null;

	struct Statement* stat = (struct Statement*)calloc(1, SSIZE(Statement));

	FileMark m = set_mark(src);

	printf("Statement begins with token:");
	printf("%s ",tokenizer_token_str(src->current_token));
	if(src->current_token.type == TOKEN_STRING) printf("str: %s", src->current_token.string);
	printf("\n");

	struct Var_Decl_List* varlist = parse_var_list(src);
	if(varlist) {
		stat->type = STAT_VAR_DECL;
		stat->var_list = varlist;

		if(eat_token(src, TOKEN_EQUAL)) {
			// parse explist
			helper_parse_varlist_explist(src, varlist);
			free_mark(&m);
			printf("Parsed variable list '=' expression list\n");
			return stat;
		}
		else
			printf("[stat] Didn't find equals, throwing away varlist...\n");
	}
	goto_mark(src, m);

	printf("[stat] Trying function call through 'parse_var'\n");
	struct Function_Call* func = null;
	struct Variable_Decl* var = parse_var(src, true, &func);
	if(func) {
		stat->type = STAT_FUNC_CALL;
		stat->func_call = func;
		printf("[stat] Got valid func\n");
		if(var && var->pre->func_call != func) { // Variable can have our func call as prefix, is fine
			if(var->name) printf("Parsed variable as well as func, %s\n", var->name);
			error(src, "Parsed func call but variable also defined");
		}
		printf("Got a function call; %s\n",func->name);
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Block* do_block = parse_do_block(src);
	if(do_block){
		stat->type = STAT_DO_BLOCK;
		stat->do_block = do_block;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct While_Block* repeat_block = parse_repeat_block(src);
	if(repeat_block){
		stat->type = STAT_REPEAT_BLOCK;
		stat->while_repeat_block = repeat_block;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	printf("[stat] trying if-block\n");
	struct If_Block* if_block = parse_if_block(src);
	if(if_block){
		stat->type = STAT_IF_BLOCK;
		stat->if_block = if_block;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Indexed_For_Block* indexed_for = parse_indexed_for(src);
	if(indexed_for){
		stat->type = STAT_FOR_INDEXED;
		stat->indexed_for = indexed_for;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Iter_For_Block* iter_for = parse_iter_for(src);
	if(iter_for){
		stat->type = STAT_FOR_IN;
		stat->iter_for_block = iter_for;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Global_Func_Decl* global_func = parse_global_func(src);
	if(global_func){
		stat->type = STAT_GLOBAL_FUNC_DECL;
		stat->global_func = global_func;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);

	struct Local_Func_Decl* local_func = parse_local_func(src);
	if(local_func){
		stat->type = STAT_LOCAL_FUNC_DECL;
		stat->local_func = local_func;
		free_mark(&m);
		return stat;
	}
	goto_mark(src, m);


	if(eat_keyword(src, "return")){
		stat->type = STAT_RETURN;
		stat->exprlist = parse_exp_list(src);
		free_mark(&m);
		return stat;
	}
	else if(eat_keyword(src, "break")){
		stat->type = STAT_BREAK;
		free_mark(&m);
		return stat;
	}
	else if(eat_keyword(src, "local")){


		// TODO local function isn't parsed here

		struct Name_List* names = parse_name_list(src);
		stat->type = STAT_LOCAL_VAR_DECL;
		stat->names = names;
		if(names && eat_token(src, TOKEN_EQUAL))
			stat->exprlist = parse_exp_list(src);

		// Accept statement if we get a name list
		if(names){
			printf("[stat] parsed local variable declaration\n");
			free_mark(&m);
			return stat;
		}
		goto_mark(src, m);
	}

	free_mark(&m);
	printf("No statement parseable\n");
	return null;
}

struct Block* parse_block(struct FileHandle* src){
	struct Block* b = (struct Block*)calloc(1, SSIZE(Block));
	b->stat_count = 0;
	printf("Parsing statement---------------\n");
	struct Statement* stat = parse_stat(src);

	struct Statement* outArr = null;
	int out_len = 0;

	while(stat != null){
		printf("Parsing statement-------------------\n");
		out_len++;
		outArr = (struct Statement*)realloc(outArr, SSIZE(Statement) * out_len);
		memcpy(&outArr[out_len-1], stat, SSIZE(Statement));

		free(stat);
		stat = parse_stat(src);
	}

	if(out_len > 0){
		struct Statement* last = &outArr[out_len-1];
		if(last->type == STAT_RETURN || last->type == STAT_BREAK){
			b->laststat = (struct Statement*)malloc(SSIZE(Statement));
			memcpy(b->laststat, &outArr[out_len-1], SSIZE(Statement));
			out_len--;
			outArr = (struct Statement*)realloc(outArr, SSIZE(Statement) * out_len);
		}
	}

	int count = out_len;
	if(b->laststat) count++;

	printf("End of statement, count: %d\n", count);
	b->stat = outArr;
	b->stat_count = out_len;
	return b;
}


struct Function_Closure* parse_func_closure(struct FileHandle* src){
	if(!eat_keyword(src, "function")) return null;
	if(!eat_token(src, TOKEN_LPAREN)) error(src, "Expected parameter list");

	struct Function_Closure* func = (struct Function_Closure*)calloc(1, SSIZE(Function_Closure));
	func->params = parse_param_list(src);
	func->block = parse_block(src);

	if(!eat_keyword(src, "end")) error(src, "Unclosed function scope");
	return func;
}

struct Field* parse_field(struct FileHandle* src) {
	struct Token peek = tokenizer_peek_next(src);
	struct Field* field = (struct Field*)calloc(1, SSIZE(Field));
	if(eat_token(src, TOKEN_LSQBRACKET)){
		field->left = parse_exp(src);
		if(!eat_token(src, TOKEN_RSQBRACKET)) error(src, "Expected closing square brace");
		if(!eat_token(src, TOKEN_EQUAL)) error(src, "Expected expression = expression");
		field->right = parse_exp(src);
		return field;
	}
	else if(is_token(src, TOKEN_IDENT) && peek.type == TOKEN_EQUAL){
		field->name = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
		strcpy(field->name, src->current_token.string);
		eat_token(src, TOKEN_IDENT);
		eat_token(src, TOKEN_EQUAL);
		// Continue parsing right hand side as normal
	}

	field->right = parse_exp(src);
	if(!field->right) error(src, "Expected expression");
	return field;
}

struct Field_List* parse_field_list(struct FileHandle* src){
	struct Field* f = null;
	int field_c = 0;

	while(1){
		struct Field* tmp = parse_field(src);
		if(!tmp) break;
		field_c++;
		if(!f) f = (struct Field*)calloc(1, SSIZE(Field));
		f = (struct Field*)realloc(f, SSIZE(Field) * field_c);
		memcpy(&f[field_c-1], tmp, SSIZE(Field));
		free(tmp);
		if(!eat_token(src, TOKEN_COMMA) && !eat_token(src, TOKEN_SEMICOLON)) break;
	}
	if(!field_c) error(src, "Expected one or more fields");

	struct Field_List* out = (struct Field_List*)malloc(SSIZE(Field_List));
	out->fields = f;
	out->field_count = field_c;
	return out;
}


/*
 * Funny enough tables are field lists with brackets around them
*/
struct Table* parse_table_cons(struct FileHandle* src){
	if(!eat_token(src, TOKEN_LBRACKET)) return null;

	struct Table* t = parse_field_list(src);

	if(!eat_token(src, TOKEN_RBRACKET)) error(src, "Expected closing brace for table constructor");
	return t;
}


struct Unary_Op* parse_un_op(struct FileHandle* src){
	struct Unary_Op* un = (struct Unary_Op*)malloc(SSIZE(Unary_Op));

	switch(src->current_token.type){
	case TOKEN_HASH:
		un->op = UOP_HASH; eat_token(src, TOKEN_HASH); break;
	case TOKEN_MINUS:
		un->op = UOP_NEG; eat_token(src, TOKEN_MINUS); break;
	case TOKEN_KEYWORD:
		if(strcmp(src->current_token.string, "not") == 0){
			un->op = UOP_NOT;
			eat_token(src, TOKEN_KEYWORD);
		} else { return null; }
		break;
	default: free(un); return null;
	}

	un->right = parse_exp_atomic(src);
	if(!un->right) error(src, "Expected expression after unary operator");
	return un;
}

int get_predecence(enum Bin_Op op){
	switch(op){
	case BOP_OR: return 0;
	case BOP_AND: return 1;
	case BOP_LES:
	case BOP_GRE:
	case BOP_LEQ:
	case BOP_GEQ:
	case BOP_EQA:
	case BOP_NEQ: return 2;
	case BOP_APP: return 3;
	case BOP_ADD:
	case BOP_SUB: return 4;
	case BOP_MUL:
	case BOP_DIV:
	case BOP_MOD: return 5;
	case BOP_POW: return 7;
	default: return -1;
	}
}

struct Binary_Op* check_precedence(struct Binary_Op* root){
	int this_pre = get_predecence(root->op);
	int right_pre = -1;
	if(root->right->type == EXP_BIN_OP){
		right_pre = get_predecence(root->right->binary_op->op);
		if(this_pre > right_pre && right_pre >= 0){
			struct Expression* right = root->right;
			root->right = right->binary_op->left;

			struct Expression* exp = (struct Expression*)calloc(1, SSIZE(Expression));
			exp->type = EXP_BIN_OP;
			exp->binary_op = root;
			right->binary_op->left = exp;
			root = right->binary_op;
			free(right);
			root->left->binary_op = check_precedence(root->left->binary_op);
		}
	}
	return root;
}

/*
			// Put the right binary operators left expression
			// into current binary operator
			bin_op->right = right->binary_op->left;

			// Wrap our binary operator into an expression and
			// assign it to right->left
			struct Expression* newExp = calloc(1, SSIZE(Expression));
			newExp->type = EXP_BIN_OP;
			newExp->binary_op = bin_op;
			right->binary_op->left = newExp;
			bin_op = right->binary_op;
			free(right); // Unwrap the expression into a binary operator, since it's
			// the new top-level expression (but we only parse bin op here)

*/

struct Binary_Op* parse_bin_op(struct FileHandle* src, bool parse_left, struct Expression* left){
	if(parse_left) left = parse_exp_atomic(src);
	if(!left && parse_left) return null;
	enum Bin_Op op;

	switch(src->current_token.type){
	case TOKEN_PLUS: op = BOP_ADD; eat_token(src, TOKEN_PLUS); break;
	case TOKEN_MINUS: op = BOP_SUB; eat_token(src, TOKEN_MINUS); break;
	case TOKEN_STAR: op = BOP_MUL; eat_token(src, TOKEN_STAR); break;
	case TOKEN_FSLASH: op = BOP_DIV; eat_token(src, TOKEN_FSLASH); break;
	case TOKEN_POW: op = BOP_POW; eat_token(src, TOKEN_POW); break;
	case TOKEN_PERC: op = BOP_MOD; eat_token(src, TOKEN_PERC); break;
	case TOKEN_DDOT: op = BOP_APP; eat_token(src, TOKEN_DDOT); break;
	case TOKEN_LESS: op = BOP_LES; eat_token(src, TOKEN_LESS); break;
	case TOKEN_GREATER: op = BOP_GRE; eat_token(src, TOKEN_GREATER); break;
	case TOKEN_LEQ: op = BOP_LEQ; eat_token(src, TOKEN_LEQ); break;
	case TOKEN_GEQ: op = BOP_GEQ; eat_token(src, TOKEN_GEQ); break;
	case TOKEN_DEQUAL: op = BOP_EQA; eat_token(src, TOKEN_DEQUAL); break;
	case TOKEN_NOTEQ: op = BOP_NEQ; eat_token(src, TOKEN_NOTEQ); break;
	case TOKEN_KEYWORD:
		if(strcmp(src->current_token.string, "and") == 0)
		{ op = BOP_AND; eat_keyword(src, "and"); break; }
		if(strcmp(src->current_token.string, "or") == 0)
		{ op = BOP_OR; eat_keyword(src, "or"); break; }
		return null;
	default: return null;
	}


	struct Expression* right = parse_exp(src);
	if(!right) return null;

	struct Binary_Op* bin_op = (struct Binary_Op*)malloc(SSIZE(Binary_Op));
	bin_op->left = left;
	bin_op->right = right;
	bin_op->op = op;

	bin_op = check_precedence(bin_op);

	return bin_op;
}

struct While_Block* parse_while_block(struct FileHandle* src){
	if(!eat_keyword(src, "while")) return null;
	struct While_Block* block = (struct While_Block*)malloc(SSIZE(While_Block));
	block->exp = parse_exp(src);
	if(!block->exp) error(src, "Expected expression");
	if(!eat_keyword(src, "do")) error(src, "Expected do-block");
	block->block = parse_block(src);
	if(!eat_keyword(src, "end")) error(src, "Expected end of do-block");
	return block;
}

struct While_Block* parse_repeat_block(struct FileHandle* src){
	if(!eat_keyword(src, "repeat")) return null;
	struct While_Block* block = (struct While_Block*)malloc(SSIZE(While_Block));
	block->block = parse_block(src);
	if(!eat_keyword(src, "until")) error(src, "Expected 'until'");
	block->exp = parse_exp(src);
	if(!block->exp) error(src, "Expected expression");
	return block;
}

struct Block* parse_do_block(struct FileHandle* src){
	if(!eat_keyword(src, "do")) return null;
	printf("Found do-block, Parsing block within\n");
	struct Block* block = parse_block(src);
	if(!eat_keyword(src, "end")) error(src, "Expected end of do-block");
	return block;
}

struct Local_Func_Decl* parse_local_func(struct FileHandle* src){
	if(!eat_keyword(src, "local")) return null;
	if(!eat_keyword(src, "function")) return null;
	struct Local_Func_Decl* func = (struct Local_Func_Decl*)malloc(SSIZE(Local_Func_Decl));
	if(!is_token(src, TOKEN_IDENT)) error(src, "Expected identifier");
	func->name = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
	strcpy(func->name, src->current_token.string);
	if(!eat_token(src, TOKEN_LPAREN)) error(src, "Expected parameterlist");
	func->params = parse_param_list(src);
	if(!eat_token(src, TOKEN_RPAREN)) error(src, "Expected end of parameterlist");
	func->block = parse_block(src);
	if(!eat_keyword(src, "end")) error(src, "Expected end of function");
	return func;
}


struct Expression* exp_test_bin(struct FileHandle* src, struct Expression* left){
	struct Binary_Op* bin = parse_bin_op(src, false, left);
	if(!bin) return left;
	struct Expression* exp = (struct Expression*)calloc(1, SSIZE(Expression));
	exp->type = EXP_BIN_OP;
	exp->binary_op = bin;
	return exp_test_bin(src, exp);
}

struct Expression* parse_exp_atomic(struct FileHandle* src){
	struct Expression* exp = (struct Expression*)calloc(1, SSIZE(Expression));
	FileMark m = set_mark(src);

	switch(src->current_token.type){
	case TOKEN_STRING:
		exp->type = EXP_STRING;
		exp->string = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
		strcpy(exp->string, src->current_token.string);
		eat_token(src, TOKEN_STRING);
		return exp;
	case TOKEN_NUMBER:
		exp->type = EXP_NUMBER;
		exp->number = src->current_token.number;
		eat_token(src, TOKEN_NUMBER);
		return exp;
	case TOKEN_TDOT:
		exp->type = EXP_VARARGS;
		eat_token(src, TOKEN_TDOT);
		return exp;
	case TOKEN_KEYWORD:
		if(eat_keyword(src, "nil")){
			exp->type = EXP_NIL;
			return exp;
		}
		else if(eat_keyword(src, "false")){
			exp->type = EXP_BOOL;
			exp->bool_value = false;
			return exp;
		}
		else if(eat_keyword(src, "true")){
			exp->type = EXP_BOOL;
			exp->bool_value = true;
			return exp;
		}
	default: break;
	}

	struct Function_Closure* func = parse_func_closure(src);
	if(func){
		exp->type = EXP_FUNC_CLO;
		exp->func_closure = func;
		free_mark(&m);
		return exp;
	}
	goto_mark(src, m);

	struct Prefix_Exp* pre = parse_prefix_exp(src);
	if(pre){
		exp->type = EXP_PREFIX_EXP;
		exp->prefix_exp = pre;
		free_mark(&m);
		return exp;
	}
	goto_mark(src, m);

	struct Table* table = parse_table_cons(src);
	if(table){
		exp->type = EXP_TABLE;
		exp->table = table;
		free_mark(&m);
		return exp;
	}
	goto_mark(src, m);


	struct Unary_Op* un_op = parse_un_op(src);
	if(un_op){
		exp->type = EXP_UN_OP;
		exp->unary_op = un_op;
		free_mark(&m);
		return exp;
	}
	goto_mark(src, m);

	free_mark(&m);
	return null;
}


struct Expression* parse_exp(struct FileHandle* src) {

	struct Expression* exp = (struct Expression*)calloc(1, SSIZE(Expression));

	FileMark m = set_mark(src);

	bool parsed = false;

	switch(src->current_token.type){
	case TOKEN_STRING:
		exp->type = EXP_STRING;
		exp->string = (char*)calloc(strlen(src->current_token.string)+1,sizeof(char));
		strcpy(exp->string, src->current_token.string);
		eat_token(src, TOKEN_STRING);
		parsed = true;
		break;
	case TOKEN_NUMBER:
		exp->type = EXP_NUMBER;
		exp->number = src->current_token.number;
		parsed = true;
		eat_token(src, TOKEN_NUMBER);
		break;
	case TOKEN_TDOT:
		exp->type = EXP_VARARGS;
		parsed = true;
		eat_token(src, TOKEN_TDOT);
		break;
	case TOKEN_KEYWORD:
		if(strcmp(src->current_token.string, "nil") == 0){
			exp->type = EXP_NIL;
			parsed = true;
			eat_token(src, TOKEN_KEYWORD);
		}else if(strcmp(src->current_token.string, "false") == 0){
			exp->type = EXP_BOOL;
			exp->bool_value = false;
			parsed = true;
			eat_token(src, TOKEN_KEYWORD);
		}else if(strcmp(src->current_token.string, "true") == 0){
			exp->type = EXP_BOOL;
			exp->bool_value = true;
			parsed = true;
			eat_token(src, TOKEN_KEYWORD);
		}
		break;
	default: break;
	}

	if(parsed){
		printf("Found an expression, checking if it's a binary one...\n");
		exp = exp_test_bin(src, exp);
		free_mark(&m);
		return exp;
	}
	goto_mark(src, m);

	printf("[exp] func closure?");

	struct Function_Closure* func = parse_func_closure(src);
	if(func){
		printf(" - yes\n");
		exp->type = EXP_FUNC_CLO;
		exp->func_closure = func;
		free_mark(&m);
		return exp_test_bin(src, exp);
	} else printf(" - no\n");
	goto_mark(src, m);

	printf("[exp] prefix exp?");

	struct Prefix_Exp* pre = parse_prefix_exp(src);
	if(pre){
		printf(" - yes\n");
		exp->type = EXP_PREFIX_EXP;
		exp->prefix_exp = pre;
		free_mark(&m);
		return exp_test_bin(src, exp);
	} else printf(" - no\n");
	goto_mark(src, m);


	printf("[exp] table constructor?");

	struct Table* table = parse_table_cons(src);
	if(table){
		printf(" - yes\n");
		exp->type = EXP_TABLE;
		exp->table = table;
		free_mark(&m);
		return exp_test_bin(src, exp);
	} else printf(" - no\n");
	goto_mark(src, m);

//	printf("[exp] binary op?");

/*	struct Binary_Op* bin_op = parse_bin_op(src, false);
	if(bin_op){
		printf(" - yes\n");
		printf("Parsed binary operator\n");
		exp->type = EXP_BIN_OP;
		exp->binary_op = bin_op;
		free_mark(&m);
		return exp;
	} else printf(" - no\n");
	goto_mark(src, m);
*/
	printf("[exp] unary op?");

	struct Unary_Op* un_op = parse_un_op(src);
	if(un_op){
		exp->type = EXP_UN_OP;
		exp->unary_op = un_op;
		free_mark(&m);
		return exp_test_bin(src, exp);
	}
	goto_mark(src, m);

	free_mark(&m);
	error(src, "Expected an expression");
	return null;

}


/*
 * Parsing NAME into variable
*/
struct Variable_Decl* parse_ident(struct FileHandle* src){
	if(src->current_token.type != TOKEN_IDENT) return null;
	struct Variable_Decl* var = (struct Variable_Decl*)calloc(1, SSIZE(Variable_Decl));
	var->name = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
	strcpy(var->name, src->current_token.string);
	eat_token(src, TOKEN_IDENT);
	return var;
}

bool var_check_func_call(struct FileHandle* src,
			struct Prefix_Exp** pre,
			struct Variable_Decl** var,
			bool nullable,
			struct Function_Call** func_out){

	if(is_token(src, TOKEN_LPAREN)){
		// Func call
		struct Args* args = parse_args(src);
		struct Function_Call* call = (struct Function_Call*)calloc(1, SSIZE(Function_Call));
		if(args){
			call->args = args;
		}
		if(*var){
			*pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
			(*pre)->type = PRE_VAR;
			(*pre)->var_decl = *var;
		}
		call->pre = *pre;
		*var = null;
		*pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
		(*pre)->type = PRE_FUNC;
		(*pre)->func_call = call;
		if(func_out) *func_out = call;
		*var = (struct Variable_Decl*)calloc(1, SSIZE(Variable_Decl));
		(*var)->pre = *pre;
		*pre = null;
		printf("[var-func-check] Found arguments indicating a function call\n");
		// Var is valid, pre is null and func_out is set
		return true;
	}


	// func call may have a ':' Name args format aswell
	if(eat_token(src, TOKEN_COLON) && is_token(src, TOKEN_IDENT)){
		struct Function_Call* call = (struct Function_Call*)calloc(1, SSIZE(Function_Call));
		call->name = (char*)calloc(strlen(src->current_token.string)+1, sizeof(char));
		strcpy(call->name, src->current_token.string);
		eat_token(src, TOKEN_IDENT);
		call->args = parse_args(src);
		if(!call->args){
			printf("no args\n");
			if(nullable) return false;
			error(src, "Expected function string argument");
		}
		if(*var){
			*pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
			(*pre)->type = PRE_VAR;
			(*pre)->var_decl = *var;
		}
		call->pre = *pre;
		*var = null;
		(*pre) = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
		(*pre)->type = PRE_FUNC;
		(*pre)->func_call = call;
		if(func_out) *func_out = call;
		*var = (struct Variable_Decl*)calloc(1, SSIZE(Variable_Decl));
		(*var)->pre = *pre;
		*pre = null;
		printf("[var-func-check] Found arguments indicating a function call\n");
		// var is valid, pre is null and func_out is set
		return true;
	}

	if(is_token(src, TOKEN_STRING)){
		struct Function_Call* call = (struct Function_Call*)calloc(1, SSIZE(Function_Call));
		if(!(*var) && !(*pre)) error(src, "Error parsing function call");
		if( *var ){
			*pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
			(*pre)->type = PRE_VAR;
			(*pre)->var_decl = *var;
		}
		call->pre = *pre;
		call->args = parse_args(src);
		*var = null;
		if(func_out) *func_out = call;
		*pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
		(*pre)->type = PRE_FUNC;
		(*pre)->func_call = call;
		*var = (struct Variable_Decl*)calloc(1, SSIZE(Variable_Decl));
		(*var)->pre = *pre;
		*pre = null;
		return true;
	}

	// No changes
	return false;

}

/*
 * Parse a variable, with an optional prefix
 * If a function call can be parsed, pointer handle to func will be set
*/
struct Variable_Decl* parse_var(struct FileHandle* src, bool nullable, struct Function_Call** func_out){
	// Variable can only be a NAME or '('exp')' followed by other things
	if(src->current_token.type != TOKEN_IDENT &&
	   src->current_token.type != TOKEN_LPAREN) {
		if(!nullable) error(src, "Expected variable");
		return null;
	}

	// Name
	struct Variable_Decl* var = parse_ident(src);
	struct Prefix_Exp* pre = null;
	bool valid_end = var != null;

	// '(' exp ')'
	if(!var) {
		// We didn't have an identifier, this means we have an expression
		// '('
		if(!nullable) assert_eat_token(src, TOKEN_LPAREN);
		if(nullable && !eat_token(src, TOKEN_LPAREN)) return null;

		// exp
		pre = (struct Prefix_Exp*)calloc(1, SSIZE(Prefix_Exp));
		pre->exp = parse_exp(src);
		pre->type = PRE_EXPR;
		var = (struct Variable_Decl*)calloc(1, SSIZE(Variable_Decl));
		var->pre = pre;
		if(!pre->exp && !nullable) error(src, "Expected expression"); // Is a must
		if(!pre->exp && nullable) return null;

		// ')'
		if(!nullable) assert_eat_token(src, TOKEN_RPAREN);
		if(nullable && !eat_token(src, TOKEN_RPAREN)) return null;

		printf("[var-test] managed to parse (exp) as prefix...\n");
	}


	bool check = true;

	// Keep checking function call and indexing operator
	// Since these can be after one-another
	while(check) {
		check = false;
		// Either pre or var is defined
		// '[' exp ']'
		if(is_token(src, TOKEN_LSQBRACKET)){
			eat_token(src, TOKEN_LSQBRACKET);

			struct Prefix_Exp* pre2 = var->pre;
			if(!var || var->pre) var = (struct Variable_Decl*)calloc(1, SSIZE(Variable_Decl));
			var->exp = parse_exp(src);
			var->pre = pre2;
			pre = pre2;

			if(nullable && !eat_token(src, TOKEN_RSQBRACKET)) return null;
			if(!nullable) assert_eat_token(src, TOKEN_RSQBRACKET);
			valid_end = true;
			check = true;
			printf("[var-test] Managed to parse indexing operator...\n");
		}

		bool is_func = var_check_func_call(src, &pre, &var, nullable, func_out);

		if(is_func) printf("[var-test] Managed to parse function call as prefix\n");

		valid_end = !is_func;

		if(is_func) check = true;
	}

	// Parse variable after dot
	if(is_token(src, TOKEN_DOT)) {
		eat_token(src, TOKEN_DOT);
		printf("[var-test] Found '.' recursing... --------------------\n");

		struct Variable_Decl* var2 = parse_var(src, nullable, func_out);

		if(!var2 && !nullable) error(src, "Expected variable");
		if(!var2 && nullable) { printf("var parsed to null\n"); return null;}

		var->after_dot_var = var2;
		valid_end = var2 != null;
	}

	if(!valid_end) printf("[var-test] Reached end without a valid variable\n");

	if(valid_end) return var;
	printf("invalid var\n");
	return null;
}


/*
 * Start parsing a source file, returning a block of code or null if none found
*/
struct Block* parse_AST(struct FileHandle* src) {
	struct Block* block = parse_block(src);
	if(!is_token(src, TOKEN_END_OF_FILE)){
		error(src, "Unexpected symbol");
	}
	return block;
}


void print_block(struct Block* block);
void print_var(struct Variable_Decl* var);
void print_var_prefix(struct Prefix_Exp* exp, bool print_dot);
void print_exp(struct Expression* exp);


void print_field(struct Field* field){
	if(!field) return;
	if(field->name) printf("%s", field->name);
	if(field->left) { print_exp(field->left); printf(" = "); }
	if(field->right) print_exp(field->right);
}

void print_table(struct Table* table){
	if(!table) return;
	printf("[table { ");
	for(int i=0;i < table->field_count;i++){
		print_field(&table->fields[i]);
		if(i < table->field_count - 1) printf(" , ");
	}
	printf(" }]");
}

void print_bin_op(struct Binary_Op* bin){
	if(!bin) return;
	printf("(");
	print_exp(bin->left);
	switch(bin->op){
	case BOP_ADD: printf(" + ");break;
	case BOP_SUB: printf(" - ");break;
	case BOP_MUL: printf(" * ");break;
	case BOP_DIV: printf(" / ");break;
	case BOP_POW: printf(" ^ ");break;
	case BOP_MOD: printf(" %% ");break;
	case BOP_APP: printf(" .. ");break;
	case BOP_LES: printf(" < ");break;
	case BOP_LEQ: printf(" <= ");break;
	case BOP_GRE: printf(" > ");break;
	case BOP_GEQ: printf(" >= ");break;
	case BOP_EQA: printf(" == ");break;
	case BOP_NEQ: printf(" ~= ");break;
	case BOP_AND: printf(" and ");break;
	case BOP_OR: printf(" or ");break;
	}
	print_exp(bin->right);
	printf(")");
}

void print_un_op(struct Unary_Op* un){
	if(!un) return;
	switch(un->op){
	case UOP_NEG: printf("-"); break;
	case UOP_NOT: printf("not "); break;
	case UOP_HASH: printf("#"); break;
	}
	print_exp(un->right);
}

void print_exp(struct Expression* exp) {
	if(!exp) return;
	switch(exp->type) {
	case EXP_NUMBER: printf("%f", exp->number); break;
	case EXP_STRING: printf("\"%s\"", exp->string); break;
	case EXP_BOOL: if(exp->bool_value){printf("true");}else{printf("false");} break;
	case EXP_VARARGS: printf("..."); break;
	case EXP_FUNC_CLO: printf("func"); break;
	case EXP_PREFIX_EXP: print_var_prefix(exp->prefix_exp, false); break;
	case EXP_TABLE: print_table(exp->table); break;
	case EXP_BIN_OP: print_bin_op(exp->binary_op); break;
	case EXP_UN_OP: print_un_op(exp->unary_op); break;
	case EXP_NIL: printf("nil"); break;
	default: printf("error"); break;
	}
}

void print_var_simple(struct Variable_Decl* var){
	if(!var) return;
	print_var_prefix(var->pre, false);
	if(var->name) printf("%s",var->name);
	if(var->exp){
		printf("[");
		print_exp(var->exp);
		printf("]");
	}
	if(var->after_dot_var){
		printf(".");
		print_var_simple(var->after_dot_var);
	}
}

void print_exp_list(struct Expression_List* explist){
	if(!explist) return;
	for(int i=0; i < explist->exp_count;i++){
		print_exp(&explist->exprs[i]);
		if(i != explist->exp_count-1) printf(",");
	}
}

void print_args(struct Args* args){
	if(!args) return;
	printf("(args: ");
	print_exp_list(args->explist);
	print_table(args->table);
	if(args->string) printf("%s",args->string);
	printf(")");
}


void print_func_call(struct Function_Call* func){
	if(!func) return;
	printf("[func-call name='");
	print_var_prefix(func->pre, false);
	if(func->name) printf(":%s",func->name);
	printf("' ");
	print_args(func->args);
	printf("]");
}

void print_var_prefix(struct Prefix_Exp* exp, bool print_dot){
	if(!exp) return;
	switch(exp->type){
	case PRE_VAR: print_var_simple(exp->var_decl); if(print_dot){printf(".");} break;
	case PRE_FUNC: print_func_call(exp->func_call); break;
	case PRE_EXPR: printf("(");print_exp(exp->exp);printf(")");break;
	default: break;
	}
}

void print_var(struct Variable_Decl* var){
	if(!var) return;
	printf("[var: '");
	print_var_prefix(var->pre, true);
	if(var->name) printf("%s", var->name);
	if(var->exp){
		printf("[");
		print_exp(var->exp);
		printf("]");
	}
	if(var->after_dot_var) {
		printf(".");
		print_var_simple(var->after_dot_var);
	}
	printf("' value='");
	struct Variable_Decl* last = var;
	while(last->after_dot_var) last = last->after_dot_var;
	print_exp(last->value);
	printf("']\n");
}

void print_var_list(struct Var_Decl_List* vars){
	if(!vars) return;

	printf("  [varlist %i]\n", vars->decl_count);
	for(int i=0; i < vars->decl_count; i++) {
		printf("   "); print_var(&vars->decl[i]);
	}
}

void print_func_name(struct Func_Name* names){
	if(!names) return;
	for(int i=0; i < names->name_c; i++){
		printf("%s",names->names[i]);
		if(i < names->name_c-1) printf(".");
	}
	if(names->lastname)
		printf(".%s",names->lastname);
}

void print_name_list(struct Name_List* names){
	if(!names) return;
	for(int i=0; i < names->name_count; i++){
		printf("%s", names->names[i]);
		if(i < names->name_count-1) printf(",");
	}
}

void print_param_list(struct Parameter_List* params){
	if(!params) return;
	print_name_list(params->names);
	printf("has varargs:%d",params->varargs_end);
}

void print_global_func(struct Global_Func_Decl* func){
	if(!func) return;

	printf("[G func name='");
	print_func_name(func->name);
	printf("' params='");
	print_param_list(func->params);
	printf("']\n");
	print_block(func->block);
	printf("[G end func]\n");
}

void print_while(struct While_Block* w_block){
	//TODO
	printf("[while]\n");
}

void print_repeat(struct While_Block* r_block){
	// TODO
	printf("[repeat]\n");
}

void print_if(struct If_Block* if_block){
	// TODO
	printf("[if]\n");
}

void print_for_indexed(struct Indexed_For_Block* index_for){
	//TODO
	printf("[for_index]\n");
}

void print_for_in(struct Iter_For_Block* iter){
	//TODO
	printf("[for_in]\n");
}

void print_local_func(struct Local_Func_Decl* lfunc_decl){
	//TODO
	printf("[local func]\n");
}

void print_local_var(struct Name_List* names, struct Expression_List* exprlist){
	if(!names) return;
	printf("[local ");
	print_name_list(names);
	if(exprlist){
		printf(" = ");
		print_exp_list(exprlist);
	}
	printf("]\n");
}


void print_stat(struct Statement* stat){
	if(!stat) return;
	printf(" [Statement]\n");
	switch(stat->type){
	case STAT_VAR_DECL: print_var_list(stat->var_list); break;
	case STAT_FUNC_CALL: print_func_call(stat->func_call);printf("\n"); break;
	case STAT_GLOBAL_FUNC_DECL: print_global_func(stat->global_func); break;
	case STAT_RETURN: printf("[return "); print_exp_list(stat->exprlist); printf("]\n"); break;
	case STAT_BREAK: printf("[break]\n"); break;
	case STAT_DO_BLOCK: print_block(stat->do_block); break;
	case STAT_WHILE_BLOCK: print_while( stat->while_repeat_block ); break;
	case STAT_REPEAT_BLOCK: print_repeat( stat->while_repeat_block ); break;
	case STAT_IF_BLOCK: print_if( stat->if_block); break;
	case STAT_FOR_INDEXED: print_for_indexed(stat->indexed_for); break;
	case STAT_FOR_IN: print_for_in(stat->iter_for_block); break;
	case STAT_LOCAL_FUNC_DECL: print_local_func(stat->local_func); break;
	case STAT_LOCAL_VAR_DECL: print_local_var(stat->names, stat->exprlist); break;
	default: printf("Unknown statement type\n");break;
	}
	printf(" [End Statement]\n");
}

void print_block(struct Block* b){
	printf("[Block]\n");
	for(int i=0; i < b->stat_count; i++)
		print_stat(&b->stat[i]);

	print_stat(b->laststat);
}


void print_AST(struct Block* b) {
	print_block(b);
}

