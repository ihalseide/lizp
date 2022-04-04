#include <stdlib.h>
#include <assert.h>
#include "eval.h"

// Always create new Val objects
Val *EvalAst(Val *ast, Val *env)
{
	if (ValIsInt(ast))
	{
		return ValMakeInt(ast->integer);
	}
	else if (ValIsSeq(ast))
	{
		Seq *astSeq = ast->sequence;
		int len = SeqLength(astSeq);
		Seq *eSeq = SeqInit(len);
		for (int i = 0; i < len; i++)
		{
			Val *e = EvalAst((Val *) SeqGet(astSeq, i), env);
			SeqAppend(eSeq, e);
		}
		return ValMakeSeq(eSeq);
	}
	else
	{
		return ast;
	}
}

static const Func functions[] =
{
	(Func){ .nameBase36 = 13441 /* add */, .numParams = 2 }, 
	(Func){ .nameBase36 = 37379 /* sub */, .numParams = 2 }, 
	(Func){ .nameBase36 = 29613 /* mul */, .numParams = 2 }, 
	(Func){ .nameBase36 = 17527 /* div */, .numParams = 2 }, 
	(Func){ .nameBase36 = 30328 /* neg */, .numParams = 1 },
};

Val *EvalDoFunction(Val *ast, Val *env)
{
	assert(ValIsSeq(ast));
	Seq *seq = ast->sequence;
	int len = SeqLength(seq);
	// [] -> []
	if (!len)
	{
		return ast;
	}
	Val *fn = SeqGet(seq, 0);
	if (!ValIsInt(fn))
	{
		// First element not a function name/id
		ValFreeRec(ast);
		return NULL;
	}
	int nameBase36 = fn->integer;
	const int numFunctions = sizeof(functions) / sizeof(functions[0]);
	for (int i = 0; i < numFunctions; i++)
	{
		Func f = functions[i];
		if (f.nameBase36 == nameBase36)
		{
			if (f.numParams == (len - 1))
			{
				Val *result;
				switch (nameBase36)
				{
					case 13441 /* add */:
						result = ValMakeInt(((Val*)SeqGet(seq, 1))->integer
								+ ((Val*)SeqGet(seq, 2))->integer);
						break;
					case 37379 /* sub */:
						result = ValMakeInt(((Val*)SeqGet(seq, 1))->integer
								- ((Val*)SeqGet(seq, 2))->integer);
						break;
					case 29613 /* mul */:
						result = ValMakeInt(((Val*)SeqGet(seq, 1))->integer
								* ((Val*)SeqGet(seq, 2))->integer);
						break;
					case 17527 /* div */:
						result = ValMakeInt(((Val*)SeqGet(seq, 1))->integer
								/ ((Val*)SeqGet(seq, 2))->integer);
						break;
					case 30328 /* neg */:
						result = ValMakeInt(-((Val*)SeqGet(seq, 1))->integer);
						break;
					default:
						// Unknown
						result = NULL;
						break;
				}
				ValFreeRec(ast);
				return result;
			}
		}
	}
	ValFreeRec(ast);
	return NULL;
}

Val *EvalVal(Val *ast, Val *env)
{
	Val *newAst = EvalAst(ast, env);
	if (ValIsSeq(newAst))
	{
		return EvalDoFunction(ast, env);
	}
	else if (ValIsInt(newAst))
	{
		return newAst;
	}
	else
	{
		// Invalid type
		assert(0 && "invalid Val type");
	}
}

