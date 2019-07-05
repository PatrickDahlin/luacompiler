#include "IRParser.h"
#include <stdlib.h>
#include <string.h> // memcpy

#define alloc(x) (x*)calloc(1,sizeof(x))

void pushNode(IRNode* parent, IRNode* subnode){
	parent->subnode = realloc(parent->subnode, parent->subnode_c+1);

	memcpy(&(parent->subnode[parent->subnode_c]),
		subnode,
		sizeof(IRNode));

	//parent->subnode[parent->subnode_c] = subnode;
	parent->subnode_c++;
}

IRNode* ParseIR(struct Block* blk) {
	return IRBlock(null, blk);
}


IRNode* IRBlock(IRNode* parent, struct Block* blk){

	IRNode* node = alloc(IRNode);
	node->type = IRBLK;
	node->parent = parent;
	node->nodeinfo = alloc(IRBlockInfo);

	// Each var decl should push a var node onto the var stack of this block
	int index = 0;
	while(index < blk->stat_count) {

		IRNode* subnode = IRStat(node, &blk->stat[index]);
		pushNode(node, subnode);

		index++;
	}

	return node;

}



IRNode* IRStat(IRNode* parent, struct Statement* stat){

	IRNode* n = alloc(IRNode);
	n->parent = parent;

	switch(stat->type){
	case STAT_VAR_DECL:
		n->type = IRVAR;

		n->nodeinfo = alloc(IRVarInfo);

		break;
	default: return null;
	}

	return n;

}
