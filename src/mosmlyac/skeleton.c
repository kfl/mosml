#include "defs.h"

char *header[] =
{
  "open Obj Parsing;",
  "prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 \"make_vect\";",
  "prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 \"set_vect_item\";",
  "",
  0
};

char *define_tables[] =
{
  "val yytables : parseTables =",
  "  ( yyact,",
  "    yytransl,",
  "    yylhs,",
  "    yylen,",
  "    yydefred,",
  "    yydgoto,",
  "    yysindex,",
  "    yyrindex,",
  "    yygindex,",
  "    YYTABLESIZE,",
  "    yytable,",
  "    yycheck );",
  0
};

void write_section(char *section[])
{
    register int i;
    register FILE *fp;

    fp = code_file;
    for (i = 0; section[i]; ++i)
    {
	++outline;
	fprintf(fp, "%s\n", section[i]);
    }
}
