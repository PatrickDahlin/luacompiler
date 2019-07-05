#include "emit_lua.h"

#include <stdio.h>

/// forward decl
void emit_var(FILE* f, struct Variable_Decl* var, bool emit_exp);
void emit_exp(FILE* f, struct Expression* exp);
void emit_block(FILE* f, struct Block* b);
void emit_func_call(FILE* f, struct Function_Call* func);
void emit_table_cons(FILE* f, struct Table* table);

///

void emit_number(FILE* f, float num){
	fprintf(f, "%f", num);
}

void emit(FILE* f, const char* src){
	fprintf(f, "%s", src);
}


void emit_prefix(FILE* f, struct Prefix_Exp* pre){
	if(!pre) return;
	switch(pre->type){
	case PRE_VAR: emit_var(f, pre->var_decl, false); break;
	case PRE_FUNC: emit_func_call(f, pre->func_call); break;
	case PRE_EXPR: emit_exp(f, pre->exp); break;
	}
}

void emit_bin_op(FILE* f, struct Binary_Op* bin){
	if(!bin) return;
	emit(f,"(");
	emit_exp(f, bin->left);
	emit(f,")");
	switch(bin->op){
	case BOP_ADD: emit(f, " + ");  break;
	case BOP_SUB: emit(f, " - ");  break;
	case BOP_MUL: emit(f, " * ");  break;
	case BOP_DIV: emit(f, " / ");  break;
	case BOP_POW: emit(f, " ^ ");  break;
	case BOP_MOD: emit(f, " %% "); break;
	case BOP_APP: emit(f, " .. "); break;
	case BOP_LES: emit(f, " < ");  break;
	case BOP_LEQ: emit(f, " <= "); break;
	case BOP_GRE: emit(f, " > ");  break;
	case BOP_GEQ: emit(f, " >= "); break;
	case BOP_EQA: emit(f, " == "); break;
	case BOP_NEQ: emit(f, " ~= "); break;
	case BOP_AND: emit(f, " and ");break;
	case BOP_OR: emit(f, " or ");  break;
	}
	emit(f, "(");
	emit_exp(f, bin->right);
	emit(f, ")");
}

void emit_un_op(FILE* f, struct Unary_Op* unop){
	if(!unop) return;
	switch(unop->op){
	case UOP_NEG: emit(f, "-"); break;
	case UOP_NOT: emit(f, "not ("); break;
	case UOP_HASH: emit(f, "#"); break;
	default: break;
	}
	emit_exp(f, unop->right);
	emit(f, ")");
}

void emit_exp(FILE* f, struct Expression* exp){
	if(!exp) return;
	switch(exp->type){
	case EXP_NIL: emit(f, "nil"); break;
	case EXP_BOOL: if(exp->bool_value) emit(f, "true"); else emit(f, "false"); break;
	case EXP_NUMBER: emit_number(f, exp->number); break;
	case EXP_STRING: emit(f, "\""); emit(f, exp->string); emit(f, "\""); break;
	case EXP_VARARGS: emit(f, "..."); break;
	case EXP_FUNC_CLO: emit(f, "func-clo"); break;
	case EXP_PREFIX_EXP: emit_prefix(f, exp->prefix_exp); break;
	case EXP_TABLE: emit_table_cons(f, exp->table); break;
	case EXP_BIN_OP: emit_bin_op(f, exp->binary_op); break;
	case EXP_UN_OP: emit_un_op(f, exp->unary_op); break;
	default: printf("error\n"); break;
	}
}

void emit_var(FILE* f, struct Variable_Decl* var, bool b_emit_exp){
	if(!var) return;
	if(var->pre) {
		emit_prefix(f, var->pre);
		if(var->name) {
			emit(f, ".");
			emit(f, var->name);
		}else if(var->exp) {
			emit(f, "[");
			emit_exp(f, var->exp);
			emit(f, "]");
		}
	}else if(var->name) {
		emit(f, var->name);
	}
	if(b_emit_exp){
		if(var->value) {
			emit(f, " = ");
			emit_exp(f, var->value);
		}
	}
}

void emit_explist(FILE* f, struct Expression_List* explist){
	if(!explist) return;
	for(int i=0; i < explist->exp_count; i++){
		emit_exp(f, &explist->exprs[i]);
		if(i+1 < explist->exp_count) emit(f, ", ");
	}
}


void emit_var_decl(FILE* f, struct Var_Decl_List* varlist){
	if(!varlist) return;
	for(int i=0; i < varlist->decl_count; i++){
		emit_var(f, &varlist->decl[i], false);
		if(i+1 < varlist->decl_count)
			emit(f, ", ");
	}
	emit(f, " = ");
	for(int i=0; i < varlist->decl_count;i++){
		if(!varlist->decl[i].value) break;
		emit_exp(f, varlist->decl[i].value);
		if(i+1 < varlist->decl_count && varlist->decl[i+1].value)
			emit(f, ", ");
	}
	emit(f, "\n");
}

void emit_field(FILE* f, struct Field* field){
	if(!field) return;
	if(field->left && field->right){
		// [ exp ] = exp
		emit(f, "[");
		emit_exp(f, field->left);
		emit(f, "] = ");
		emit_exp(f, field->right);
	} else if(field->left && !field->right) {
		// exp
		emit_exp(f, field->left);
	} else if(field->name && field->right) {
		// Name = exp
		emit(f, field->name);
		emit(f, " = ");
		emit_exp(f, field->right);
	}
}

void emit_table_cons(FILE* f, struct Table* table){
	if(!table) return;
	emit(f, "{ ");
	for(int i = 0; i < table->field_count; i++){
		emit_field(f, &table->fields[i]);
		if(i+1 < table->field_count) emit(f, ", ");
	}
	emit(f, " }");
}

void emit_args(FILE* f, struct Args* args){
	if(!args) return;
	if(args->explist){
		emit(f, "(");
		emit_explist(f, args->explist);
		emit(f, ")");
	}else if(args->table) {
		emit_table_cons(f, args->table);
	}else if(args->string) {
		emit(f, " \"");
		emit(f, args->string);
		emit(f, "\"");
	}
}

void emit_func_call(FILE* f, struct Function_Call* func){
	if(!func) return;
	if(func->pre){
		emit_prefix(f, func->pre);
		if(func->name){
			emit(f, ":");
			emit(f, func->name);
			emit(f, " ");
		}
	}
	emit_args(f, func->args);
}

void emit_do_block(FILE* f, struct Block* do_block){
	if(!do_block) return;
	emit(f, "do\n");
	emit_block(f, do_block);
	emit(f, "end\n");
}

void emit_while_block(FILE* f, struct While_Block* block){
	if(!block) return;
	emit(f, "while ");
	emit_exp(f, block->exp);
	emit(f, " do\n");
	emit_block(f, block->block);
	emit(f, "end\n");
}

void emit_repeat_block(FILE* f, struct While_Block* block){
	if(!block) return;
	emit(f, "repeat\n");
	emit_block(f, block->block);
	emit(f, "until ");
	emit_exp(f, block->exp);
	emit(f, "\n");
}

void emit_if_block(FILE* f, struct If_Block* if_block){
	if(!if_block) return;
	emit(f, "if ");
	emit_exp(f, if_block->if_exp);
	emit(f, " then\n");
	emit_block(f, if_block->if_block);
	if(if_block->elif_count > 0){
		for(int i=0; i < if_block->elif_count; i++){
			emit(f, "elseif ");
			emit_exp(f, if_block->elif_blocks[i].if_exp);
			emit(f, " then\n");
			emit_block(f, if_block->elif_blocks[i].if_block);
		}
	}
	if(if_block->else_block != null){
		emit(f, "else\n");
		emit_block(f, if_block->else_block);
	}
	emit(f, "end\n");
}

void emit_for_index(FILE* f, struct Indexed_For_Block* for_block){
	if(!for_block) return;
	emit(f, "for ");
	if(!for_block->index_var->name) {
		printf("Error; indexer variable doesn't have a valid name!\n");
		//exit(-1);
	}
	emit(f, for_block->index_var->name);
	emit(f, " = ");
	emit_exp(f, for_block->index_var->value);
	emit(f, ", ");
	emit_exp(f, for_block->limit);
	if(for_block->step) {
		emit(f, ", ");
		emit_exp(f, for_block->step);
	}
	emit(f, "do\n");
	emit_block(f, for_block->block);
	emit(f, "end\n");
}

void emit_namelist(FILE* f, struct Name_List* namelist){
	if(!namelist) return;
	for(int i=0; i < namelist->name_count; i++){
		emit(f, namelist->names[i]);
		if(i < namelist->name_count-1) emit(f, ", ");
	}
}

void emit_for_in(FILE* f, struct Iter_For_Block* for_in){
	if(!for_in) return;
	emit(f, "for ");
	emit_namelist(f, for_in->names);
	emit(f, " in ");
	emit_explist(f, for_in->explist);
	emit(f, " do\n");
	emit_block(f, for_in->block);
	emit(f, "end\n");
}

void emit_paramlist(FILE* f, struct Parameter_List* params){
	if(!params) return;
	emit(f, "(");
	emit_namelist(f, params->names);
	if(params->varargs_end) emit(f, "...");
	emit(f, ")");
}

void emit_local_func(FILE* f, struct Local_Func_Decl* func){
	if(!func) return;
	emit(f, "local function ");
	emit(f, func->name);
	emit_paramlist(f, func->params);
	emit(f, "\n");
	emit_block(f, func->block);
	emit(f, "end\n");
}

void emit_func_name(FILE* f, struct Func_Name* name){
	if(!name) return;
	for(int i=0; i < name->name_c; i++){
		emit(f, name->names[i]);
		if(i < name->name_c-1) emit(f, ".");
	}
	if(name->lastname){
		emit(f, ":");
		emit(f, name->lastname);
	}
}

void emit_global_func(FILE* f, struct Global_Func_Decl* func){
	if(!func) return;
	emit(f, "function ");
	emit_func_name(f, func->name);
	emit_paramlist(f, func->params);
	emit(f, "\n");
	emit_block(f, func->block);
	emit(f, "end\n");
}

void emit_local_var_decl(FILE* f, struct Name_List* names, struct Expression_List* exprs){
	if(!names) return;
	emit(f, "local ");
	emit_namelist(f, names);
	if(exprs){
		emit(f, " = ");
		emit_explist(f, exprs);
	}
	emit(f, "\n");
}

void emit_stat(FILE* f, struct Statement* s){
	if(!s) return;
	switch(s->type){
	case STAT_VAR_DECL:
		emit_var_decl(f, s->var_list);
	break;
	case STAT_FUNC_CALL:
		emit_func_call(f, s->func_call); emit(f, "\n");
	break;
	case STAT_DO_BLOCK:
		emit_do_block(f, s->do_block);
	break;
	case STAT_WHILE_BLOCK:
		emit_while_block(f, s->while_repeat_block);
	break;
	case STAT_REPEAT_BLOCK:
		emit_repeat_block(f, s->while_repeat_block);
	break;
	case STAT_IF_BLOCK:
		emit_if_block(f, s->if_block);
	break;
	case STAT_FOR_INDEXED:
		emit_for_index(f, s->indexed_for);
	break;
	case STAT_FOR_IN:
		emit_for_in(f, s->iter_for_block);
	break;
	case STAT_LOCAL_FUNC_DECL:
		emit_local_func(f, s->local_func);
	break;
	case STAT_GLOBAL_FUNC_DECL:
		emit_global_func(f, s->global_func);
	break;
	case STAT_LOCAL_VAR_DECL:
		emit_local_var_decl(f, s->names, s->exprlist);
	break;
	case STAT_RETURN:
		emit(f, "return ");
		emit(f, "(");
		emit_explist(f, s->exprlist);
		emit(f, ")\n");
	break;
	case STAT_BREAK:
		emit(f, "break\n");
	break;
	}
}

void emit_block(FILE* f, struct Block* b){
	if(!b) return;
	for(int i = 0; i < b->stat_count; i++){
		emit_stat(f, &b->stat[i]);
	}
	if(b->laststat) emit_stat(f, b->laststat);
}


void emit_lua_source(char* outfile, struct Block* b){
	FILE* file = fopen(outfile, "wb");
	emit_block(file, b);
	fclose(file);
}
