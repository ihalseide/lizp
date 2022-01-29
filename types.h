typedef struct string String;
struct string
{
	int length;
	char *start;
};

enum Cell_kind
{
	T_INT,
	T_STRING,
	T_SYMBOL,
	T_C_FUNCTION,
	T_PAIR,
	_CELL_KIND_COUNT,
};

typedef struct cell Cell;
struct cell
{
	enum Cell_kind kind;
	union
	{
		int num;
		Cell * sym;             // Symbol points to string cell
		String str;             // String contains an actual string value
		Cell *(*c_func)(Cell*); // C function
		struct                  // Pair
		{
			Cell * first;
			Cell * rest;
			char variant;       // Type of parens used to write the list
		};
	};
};

