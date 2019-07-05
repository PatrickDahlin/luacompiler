#pragma once

#include "tokenizer.h"

struct Block;
struct Statement;
struct Expression;

struct Block* parse_AST(struct FileHandle* src);

void print_AST(struct Block* blk);

enum Exp_Type {
	EXP_NIL,
	EXP_BOOL,
	EXP_NUMBER,
	EXP_STRING,
	EXP_VARARGS,
	EXP_FUNC_CLO,
	EXP_PREFIX_EXP,
	EXP_TABLE,
	EXP_BIN_OP,
	EXP_UN_OP
};

// @unused?
enum Var_Type {
	VAR_VAR, VAR_CONST
};


enum Bin_Op {
	BOP_ADD,
	BOP_SUB,
	BOP_MUL,
	BOP_DIV,
	BOP_POW,
	BOP_MOD,
	BOP_APP, // Append, '..'
	BOP_LES, // Less than '<'
	BOP_LEQ, // Less or equal '<='
	BOP_GRE, // Greater '>'
	BOP_GEQ, // Greater or equal '>='
	BOP_EQA, // Equal '=='
	BOP_NEQ, // Not equal '~='
	BOP_AND,
	BOP_OR
};

enum Un_Op {
	UOP_NEG,
	UOP_NOT,
	UOP_HASH
};


enum Statement_Type {
	STAT_VAR_DECL,
	STAT_FUNC_CALL,
	STAT_DO_BLOCK,
	STAT_WHILE_BLOCK,
	STAT_REPEAT_BLOCK,
	STAT_IF_BLOCK,
	STAT_FOR_INDEXED,
	STAT_FOR_IN,
	STAT_LOCAL_FUNC_DECL,
	STAT_GLOBAL_FUNC_DECL,
	STAT_LOCAL_VAR_DECL,
	STAT_RETURN,
	STAT_BREAK
};


// namelist
struct Name_List {
	char** names;
	int name_count;
};

// explist
struct Expression_List {
	struct Expression* exprs;
	int exp_count;
};

// Parameter list is a namelist where we can have a varargs parameter at the end
struct Parameter_List {
	struct Name_List* names;
	bool varargs_end;
};

enum Prefix_Exp_Type {
	PRE_VAR,
	PRE_FUNC,
	PRE_EXPR
};

// Forward declarations
struct Variable_Decl;
struct Function_Call;


struct Prefix_Exp {
	enum Prefix_Exp_Type type;

	union {
		struct Variable_Decl* var_decl;
		struct Function_Call* func_call;
		struct Expression* exp;
	};
};

// There are only 2 out of the first 3 (or 1 if only Name) valid pointers in this struct which defines
// how it should be interpreted, name + prefix means name is an access
// to prefix members. prefix + exp means access to prefix member by indexing
// operator. If only name is defined then it's a variable name


// var
struct Variable_Decl {

	// Two out of these three are valid, or just name
	char* name;
	struct Prefix_Exp* pre;
	struct Expression* exp;


	// Value can be null, meaning variable is set to nil
	// this operation is basically a useless noop
	struct Expression* value;

	// This points to the last prefix expression that encountered a '.'
	// at which point this variable was parsed
	//struct Prefix_Exp* pre_prefix;
	struct Variable_Decl* after_dot_var;
};


struct Function_Call {
	char* name;
	struct Prefix_Exp* pre;
	struct Args* args;
};


// varlist
struct Var_Decl_List {
	struct Variable_Decl* decl;
	int decl_count;
};

// function
struct Function_Closure {
	struct Parameter_List* params; // Param list can be NULL if no params found
	struct Block* block;
};


// Func Name is used for global function declarations
// These can have 'somevar.mytable.myfunc:something' as
// an example, where 'something' is lastname in the struct
// All other '.' separated names are stored in names
struct Func_Name {
	char** names;
	int name_c;
	char* lastname;
};

struct Local_Func_Decl {
	char* name;
	struct Parameter_List* params;
	struct Block* block;
};

struct Global_Func_Decl {
	struct Func_Name* name;
	struct Parameter_List* params;
	struct Block* block;
};

// Fields can have 2 expressions, one expr or name = expr
struct Field {
	struct Expression* left;
	struct Expression* right;
	char* name;
};

struct Field_List {
	struct Field* fields;
	int field_count;
};
// Instead of making a duplicate struct, just rename
// Table to Field_List
#define Table Field_List


// Only one of these pointers will be defined for args
// args
struct Args {
	struct Expression_List* explist;
	struct Table* table;
	char* string;
};




struct Block {
	struct Statement* stat;
	int stat_count;
	struct Statement* laststat;
};

struct Binary_Op {
	struct Expression* left;
	enum Bin_Op op;
	struct Expression* right;
};

struct Unary_Op {
	enum Un_Op op;
	struct Expression* right;
};

struct Expression {
	enum Exp_Type type;

	union {
		struct Function_Closure* func_closure;
		bool bool_value;
		float number;
		char* string;
		struct Function_Call* func_call;
		struct Binary_Op* binary_op;
		struct Unary_Op* unary_op;
		struct Prefix_Exp* prefix_exp;
		struct Table* table;
	};
};


struct While_Block {
	struct Expression* exp;
	struct Block* block;
};

struct If_Block {
	struct Expression* if_exp;
	struct Block* if_block;

	struct If_Block* elif_blocks;
	int elif_count;

	struct Block* else_block; // optional
};

struct Indexed_For_Block {
	struct Variable_Decl* index_var;
	struct Expression* limit;
	struct Expression* step;
	struct Block* block;
};

struct Iter_For_Block {
	struct Name_List* names;
	struct Expression_List* explist;
	struct Block* block;
};


// stat
struct Statement {
	enum Statement_Type type;

	union {
		struct Var_Decl_List* var_list;
		struct Function_Call* func_call;
		// Dummuy scoping block around a piece of code
		struct Block* do_block;
		// While (exp) do (block)
		// Also used for Repeat (block) do (exp)
		struct While_Block* while_repeat_block;
		struct If_Block* if_block;
		struct Indexed_For_Block* indexed_for;
		struct Iter_For_Block* iter_for_block;
		struct Local_Func_Decl* local_func;
		struct Global_Func_Decl* global_func;
		struct Expression_List* exprlist;
	};
	// In the case of local variable declaration we aren't using var_list
	// Hence we need both Name_List and optionally Expression_List pointers at the same time
	// @Wrapinstruct?
	struct Name_List* names;
};
