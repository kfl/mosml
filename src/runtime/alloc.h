#ifndef _alloc_
#define _alloc_


#include "misc.h"
#include "mlvalues.h"

EXTERN value alloc(mlsize_t, tag_t);
EXTERN value alloc_tuple(mlsize_t);
EXTERN value alloc_string(mlsize_t);
EXTERN value alloc_final(mlsize_t, final_fun, mlsize_t, mlsize_t);
EXTERN value copy_string(char *);
EXTERN value copy_string_array(char **);
EXTERN value copy_double(double);
EXTERN value alloc_array(value (*funct) (char *), char ** array);
EXTERN int convert_flag_list(value, int *);


#endif /* _alloc_ */
