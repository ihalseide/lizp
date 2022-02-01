enum Cell_kind
{
	T_INT,
	T_STRING,
	T_SYMBOL,
	T_FUNCTION,
	T_C_FUNCTION,
	T_PAIR,
	_CELL_KIND_COUNT,
};

// Pairs & Lists:
// Pair = (first . rest)
// List = () = (NULL . NIL)
//      | (cell . List)
//      | (cell . NIL)

typedef struct cell Cell;
struct cell
{
	int free;
	enum Cell_kind kind;           // Type of cell
	union
	{
		int as_int;                // Integer value
		Cell *(*as_c_func)(Cell*); // C function value
		Cell * as_symbol;          // Symbol name value (pointer to string cell)
		struct                     // String value
		{
			char *start;
			int length;
		} as_str;
		struct
		{
			Cell *params;
			Cell *body;
		} as_func;
		struct                     // Pair/list value
		{
			Cell * first;
			Cell * rest;
			char variant;          // Type of parens used to write the list
		} as_pair;
	};
};

