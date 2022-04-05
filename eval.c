#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "eval.h"

#include "lizp.h"

static Val *EvalApply(Seq *seq)
{
	if (!seq)
	{
		return ValMakeSeq(NULL);
	}
	Val *fn = (Val*)SeqGet(seq, 0);
	// First must ve a valid function id number (a base36 name)
	if (!fn || !ValIsInt(fn))
	{
		return NULL;
	}
	int nameBase36 = fn->integer;
	int numArgs = SeqLength(seq) - 1;
	Val *result = NULL;
	switch (nameBase36)
	{
		case 13441 /* add */:
			if (numArgs == 2)
			{
				result = ValMakeInt(((Val*)SeqGet(seq, 1))->integer
						+ ((Val*)SeqGet(seq, 2))->integer);
			}
			break;
		case 37379 /* sub */:
			if (numArgs == 2)
			{
				result = ValMakeInt(((Val*)SeqGet(seq, 1))->integer
						- ((Val*)SeqGet(seq, 2))->integer);
			}
			break;
		case 29613 /* mul */:
			if (numArgs == 2)
			{
				result = ValMakeInt(((Val*)SeqGet(seq, 1))->integer
						* ((Val*)SeqGet(seq, 2))->integer);
			}
			break;
		case 17527 /* div */:
			if (numArgs == 2)
			{
				result = ValMakeInt(((Val*)SeqGet(seq, 1))->integer
						/ ((Val*)SeqGet(seq, 2))->integer);
			}
			break;
		case 30328 /* neg */:
			if (numArgs == 1)
			{
				result = ValMakeInt(-((Val*)SeqGet(seq, 1))->integer);
			}
			break;
		default:
			// Unknown
			fprintf(stderr, "EVAL: unknown function: #%d\n", nameBase36);
			break;
	}
	return result;
}

// Always create new Val objects
Val *EvalAst(Val *ast, Val *env)
{
	if (!ast)
	{
		return NULL;
	}
	else if (ValIsInt(ast))
	{
		return ValMakeInt(ast->integer);
	}
	else if (ValIsSeq(ast))
	{
		Seq *seq = ast->sequence;
		// NULL Seq evaluates to itself. A.k.a: [] -> []
		if (!seq)
		{
			return ast;
		}
		// Evalulate sub-trees in ast
		Seq *evSeq = NULL;
		while (seq)
		{
			SeqAppend(&evSeq, EvalAst((Val*)SeqVal(seq), NULL));
			seq = SeqNext(seq);
		}
		// Apply this sequence
		return EvalApply(evSeq);
	}
	else
	{
		assert(0 && "invalid ast Val");
	}
}

