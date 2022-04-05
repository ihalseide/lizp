#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "eval.h"

#include "lizp.h"
#include "printer.h"

// Get value for key in this type of seq:
// [[key1 value1] [key2 value2] [key3 value3]...]
static Val *Assoc(Seq *seq, int key)
{
	while (seq)
	{
		Val *entry = SeqVal(seq);
		assert(entry);
		assert(ValIsSeq(entry));
		assert(ValIsSeq(entry));
		Val *entryKey = SeqVal(entry->sequence);
		assert(ValIsInt(entryKey));
		if (entryKey->integer == key)
		{
			Seq *val = SeqNext(entry->sequence);
			assert(val);
			return SeqVal(val);
		}
		seq = SeqNext(seq);
	}
	return NULL;
}

// Apply function or macro
static Val *EvalApply(Seq *seq, Seq **env)
{
	if (!seq)
	{
		return ValMakeSeq(NULL);
	}
	Val *fn = (Val*)SeqGet(seq, 0);
	// First must ve a valid function id number (a base36 name)
	if (!fn || !ValIsInt(fn))
	{
		LizpError(LE_APPLY_NOT_FUNCTION);
	}
	int nameBase36 = fn->integer;
	int numArgs = SeqLength(seq) - 1;
	switch (nameBase36)
	{
		case 13441 /* add */:
			if (numArgs == 2)
			{
				return ValMakeInt(((Val*)SeqGet(seq, 1))->integer
						+ ((Val*)SeqGet(seq, 2))->integer);
			}
			break;
		case 37379 /* sub */:
			if (numArgs == 2)
			{
				return ValMakeInt(((Val*)SeqGet(seq, 1))->integer
						- ((Val*)SeqGet(seq, 2))->integer);
			}
			break;
		case 29613 /* mul */:
			if (numArgs == 2)
			{
				return ValMakeInt(((Val*)SeqGet(seq, 1))->integer
						* ((Val*)SeqGet(seq, 2))->integer);
			}
			break;
		case 17527 /* div */:
			if (numArgs == 2)
			{
				return ValMakeInt(((Val*)SeqGet(seq, 1))->integer
						/ ((Val*)SeqGet(seq, 2))->integer);
			}
			break;
		case 30328 /* neg */:
			// Negate number
			if (numArgs == 1)
			{
				return ValMakeInt(-((Val*)SeqGet(seq, 1))->integer);
			}
			break;
		case 1086854 /* name */:
			// Print number as name
			if (numArgs == 1)
			{
				Val *arg1 = (Val*)SeqGet(seq, 1);
				int n = arg1->integer;
				char out[64];
				int count = PrintInt(n, out, sizeof(out), false, 36, false);
				out[count] = '\0';
				printf("%s", out);
				return NULL;
			}
			break;
		case 1004141 /* list */:
			return ValMakeSeq(SeqNext(seq));
			break;
		case 45101858 /* quote (macro) */:
			return SeqGet(seq, 1);
			break;
		case 27749 /* let (macro) */:
			if (numArgs == 2)
			{
				Val *arg1 = (Val*)SeqGet(seq, 1);
				Val *arg2 = EvalAst((Val*)SeqGet(seq, 2), env);
				Seq *p = SeqInit(arg1, SeqInit(arg2, NULL));
				Val *pair = ValMakeSeq(p);
				SeqPush(env, pair);
				return arg2;
			}
			break;
		case 21269 /* get (macro) */:
			if (numArgs == 1)
			{
				Val *arg1 = (Val*)SeqGet(seq, 1);
				Val *val1 = Assoc(*env, arg1->integer);
				if (val1)
				{
					return val1;
				}
				else
				{
					LizpError(LE_UNKNOWN_SYM);
				}
			}
			break;
		case 492 /* do (macro) */:
			if (numArgs)
			{
				Seq *pAst = seq;
				Val *v;
				while (pAst)
				{
					v = EvalAst(SeqVal(pAst), env);
					pAst = SeqNext(pAst);
				}
				return v;
			}
			else
			{
				return ValMakeSeq(NULL);
			}
			break;
		default:
			// Unknown
			LizpError(LE_UNKNOWN_FUNCTION);
			break;
	}
	LizpError(LE_NO_FUNCTION);
}

bool EvalIsMacro(Seq *seq)
{
	assert(seq);
	Val *first = (Val*)SeqVal(seq);
	if (!ValIsInt(first))
	{
		return false;
	}
	switch (first->integer)
	{
		case 27749 /* let (macro) */:
		case 21269 /* get (macro) */:
		case 45101858 /* quote (macro) */:
		case 492 /* do (macro) */:
			return true;
		default:
			return false;
	}
}

// Always create new Val objects
Val *EvalAst(Val *ast, Seq **env)
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
		Seq *evSeq;
		if (EvalIsMacro(seq))
		{
			// Do not evaluate sub trees if macro
			evSeq = seq;
		}
		else
		{
			// Evalulate sub-trees in ast
			evSeq = NULL;
			while (seq)
			{
				SeqAppend(&evSeq, EvalAst((Val*)SeqVal(seq), NULL));
				seq = SeqNext(seq);
			}
		}
		// Apply this sequence
		return EvalApply(evSeq, env);
	}
	else
	{
		LizpError(LE_INVALID_VAL);
	}
}

