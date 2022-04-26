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
// Must not modify/touch/share structure with the original seq
static Val *EvalApply(Seq *seq, Seq **env)
{
    if (!seq)
    {
        return ValMakeSeq(NULL);
    }

    Val *fn = (Val*)SeqGet(seq, 0);
    // First must be a valid function id number (a base36 name)
    if (!ValIsInt(fn))
    {
        LizpError(LE_APPLY_NOT_FUNCTION);
    }
    int nameBase36 = fn->integer;
    int numArgs = SeqLength(seq) - 1;
    switch (nameBase36)
    {
        case 43274297:
            // [print expr]
            if (numArgs == 1)
            {
                PRINT((Val*)SeqGet(seq, 1), 1);
                return NULL;
            }
            break;
        case 13441:
            // [add x y]
            if (numArgs == 2)
            {
                return ValMakeInt(((Val*)SeqGet(seq, 1))->integer
                        + ((Val*)SeqGet(seq, 2))->integer);
            }
            break;
        case 37379:
            // [sub x y]
            if (numArgs == 2)
            {
                return ValMakeInt(((Val*)SeqGet(seq, 1))->integer
                        - ((Val*)SeqGet(seq, 2))->integer);
            }
            break;
        case 29613:
            // [mul x y]
            if (numArgs == 2)
            {
                return ValMakeInt(((Val*)SeqGet(seq, 1))->integer
                        * ((Val*)SeqGet(seq, 2))->integer);
            }
            break;
        case 17527:
            // [div x y]
            if (numArgs == 2)
            {
                int x = ((Val*)SeqGet(seq, 1))->integer;
                int y = ((Val*)SeqGet(seq, 2))->integer;
                if (y == 0)
                {
                    LizpError(LE_DIV_ZERO);
                }
                return ValMakeInt(x / y);
            }
            break;
        case 30328:
            // [neg x]
            // Negate number
            if (numArgs == 1)
            {
                return ValMakeInt(-((Val*)SeqGet(seq, 1))->integer);
            }
            break;
        case 1086854:
            // [name x]
            // Print number as name
            if (numArgs == 1)
            {
                Val *arg1 = (Val*)SeqGet(seq, 1);
                if (arg1->kind == CK_INT)
                {
                    char out[64];
                    int count = PrintInt(arg1->integer, out, sizeof(out), false, 36, false);
                    out[count] = '\0';
                    printf("%s", out);
                    return NULL;
                }
            }
            break;
        case 1004141:
            // [list ...]
            return ValMakeSeq(SeqCopy(SeqNext(seq)));
        case 45101858:
            // [quote expr]
            if (numArgs == 1)
            {
                return ValCopy(SeqGet(seq, 1));
            }
            break;
        case 27749:
            // [let k v]
            if (numArgs == 2)
            {
                assert(0 && "not implemented");
            }
            break;
        case 21269:
            // [get k]
            if (numArgs == 1)
            {
                Val *arg1 = (Val*)SeqGet(seq, 1);
                Val *val1 = Assoc(*env, arg1->integer);
                if (val1)
                {
                    return ValCopy(val1);
                }
                else
                {
                    LizpError(LE_UNKNOWN_SYM);
                }
            }
            break;
        case 492:
            // [do ...]
            if (numArgs)
            {
                Seq *pAst = seq;
                Val *v;
                while (pAst)
                {
                    v = EvalAst(SeqVal(pAst), env);
                    pAst = SeqNext(pAst);
                    if (pAst)
                    {
                        ValFreeAll(v);
                    }
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
        case 492:      // do
        case 21269:    // get
        case 27749:    // let
        case 45101858: // quote
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
        return ValCopy(ast);
    }
    else if (ValIsSeq(ast))
    {
        Seq *seq = ast->sequence;
        // NULL Seq evaluates to itself. A.k.a: [] -> []
        if (!seq)
        {
            return ValCopy(ast);
        }
        // evSeq = evaluated sequence
        Seq *evSeq;
        if (EvalIsMacro(seq))
        {
            // Do not evaluate subtrees if macro
            evSeq = seq;
        }
        else
        {
            // Evalulate sub-trees in ast
            evSeq = NULL;
            while (seq)
            {
                SeqAppend(&evSeq, EvalAst((Val*)SeqVal(seq), env));
                seq = SeqNext(seq);
            }
        }
        // Apply this sequence
        Val *result = EvalApply(evSeq, env);
        // Free evaluated value and return
        ValFreeAll(ValMakeSeq(evSeq));
        return result;
    }
    else
    {
        LizpError(LE_INVALID_VAL);
    }
}

