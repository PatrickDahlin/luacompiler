#pragma once

#include "parser.h"
#include <stdint.h>


/*

Convert our generated AST to an IR representation with type info where applicable
Also do compile-time error-checking at this stage for possible nil values and adding in nil-checks

Constants are parsed as anonymous variable declarations

*/

typedef enum {
	IRUNDEF = 0x0,
	IRVAR = 0x1,
	IROP = 0x8,
	IRBLK = 0x16,
} IRNodeType;


typedef enum {
	IRADD = 0x0,
	IRSUB = 0x1,
	IRMUL = 0x2,
	IRDIV = 0x3
}IRBinOp;

/*
NODE TYPES
*/

typedef struct {
	IRNodeType left;
	IRNodeType right;
	void* leftval;
	void* rightval;
} IRBinOpNode;

/*
NODE INFO
*/
typedef struct {
	IRVarInfo* scoped_vars;
	uint16_t scope_var_c;
} IRBlockInfo;

typedef enum
{ IRINT, IRFLOAT, IRSTR, IRTABLE }
VarType;

typedef struct {
	char* name;
	VarType type;
	bool constant;
	void* value;
} IRVarInfo;

/*
BASE NODE
*/

typedef struct {
	IRNodeType type;
	void* parent;
	void* nodeinfo;
	void* subnode;
	uint16_t subnode_c;
} IRNode;

typedef struct {
	IRNode* base_block;
	IRVarInfo* globals;
	uint16_t global_c;
} IRTree;



IRNode* ParseIR(struct Block* blk);


IRNode* IRBlock(IRNode* parent, struct Block* blk);
IRNode* IRStat(IRNode* parent, struct Statement* stat);
