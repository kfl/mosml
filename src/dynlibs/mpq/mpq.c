/* mpq.c -- Moscow ML interface interface to the PostgreSQL pq library.
   sestoft@dina.kvl.dk 1998-10-29, 1998-11-04 */

#include <stdlib.h>

/* Access to the camlrunm/Moscow ML runtime data representation: */

#include <mlvalues.h>
#include <memory.h>
#include <fail.h>
#include <alloc.h>

/* The following is a hack to avoid a stupid define-conflict (C #&%$@!) */

#define int32 int
#define uint32 int

/* Access to PostgreSQL stuff: */

#include "libpq-fe.h"

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

/* A connection pgconn_ is an abstract object

              header with Abstract_tag
	      0: PGconn pointer

   whose component 0 is a pointer to a PGconn value.  A PostgreSQL
   database must be closed explicitly for its resources to be deallocated.


   A query result pgresult_ is a finalized object: a pair, 

              header with Final_tag
	      0: finalization function pgresult_finalize
	      1: PGresult pointer

   whose component 0 is a pointer to the finalizing function
   pgresult_finalize, and whose component 1 is a pointer to a PGresult
   value.  The finalization function will apply PQclear to the second
   component of the pair: */

#define PGconn_val(x) ((PGconn*)(Field(x, 0)))

#define PGresult_val(x) ((PGresult*)(Field(x, 1)))

value pgconn_alloc(PGconn* conn)
{ 
  value res = alloc(1, Abstract_tag);
  initialize(&Field(res, 0), (value)conn);
  return res;
}
 
void pgresult_finalize(value pgresval)
{ 
  PGresult* pgres = PGresult_val(pgresval);
  PQclear(pgres);
}

/* When the pgresult_ value becomes unreachable from the ML process,
   it will be garbage-collected, and PGresult_finalize() will be
   called on the pointer to the Pgresult struct to deallocate it.  */

value pgresult_alloc(PGresult* pgres)
{ 
  value res = alloc_final(2, &pgresult_finalize, 1, 10000);
  initialize(&Field(res, 1), (value)pgres);
  return res;
}

/* Return NONE if s==NULL, return SOME(s) otherwise: */

value Val_stringornull(char* s) 
{
  if (s == NULL)
    return NONE;
  else { /* return SOME(s) */
    value res;
    Push_roots(r, 1);
    r[0] = copy_string(s);
    res = alloc(1, SOMEtag); 
    Field(res, 0) = r[0];
    Pop_roots();
    return res;
  }
}

/* Type: ML string option -> C char* */
char* StringOrNull_val(value v)
{
  if (v == NONE)
    return (char*)NULL;
  else
    return String_val(Field(v, 0));
}

/* ML type : 6-element record -> pgconn_ */
EXTERNML value pq_setdb(value args) 
{
  char* dbhost    = StringOrNull_val(Field(args, 0));
  char* dbname    = StringOrNull_val(Field(args, 1));
  char* dboptions = StringOrNull_val(Field(args, 2));
  char* dbport    = StringOrNull_val(Field(args, 3));
  char* dbpwd     = StringOrNull_val(Field(args, 4));
  char* dbtty     = StringOrNull_val(Field(args, 5));
  char* dbuser    = StringOrNull_val(Field(args, 6));
  return (value)(pgconn_alloc(PQsetdbLogin(dbhost, dbport, dboptions, dbtty, 
					   dbname, dbuser, dbpwd)));
}

/* ML type : pgconn_ -> string */
EXTERNML value pq_db(value conn) 
{
  return copy_string(PQdb(PGconn_val(conn)));
}

/* ML type : pgconn_ -> string */
EXTERNML value pq_host(value conn) 
{
  return Val_stringornull(PQhost(PGconn_val(conn)));
}

/* ML type : pgconn_ -> string */
EXTERNML value pq_options(value conn) 
{
  return copy_string(PQoptions(PGconn_val(conn)));
}

/* ML type : pgconn_ -> string */
EXTERNML value pq_port(value conn) 
{
  return copy_string(PQport(PGconn_val(conn)));
}

/* ML type : pgconn_ -> string */
EXTERNML value pq_tty(value conn) 
{
  return copy_string(PQtty(PGconn_val(conn)));
}

/* ML type : pgconn_ -> bool */
EXTERNML value pq_status(value conn) 
{
  return Val_bool(PQstatus(PGconn_val(conn)) == CONNECTION_OK);
}

/* ML type : pgconn_ -> string option */
EXTERNML value pq_errormessage(value conn) 
{
  char* msg = PQerrorMessage(PGconn_val(conn));
  if (msg == NULL || msg[0] == '\0')
    msg = NULL;
  return Val_stringornull(msg);
}

/* ML type : pgconn_ -> unit */
EXTERNML value pq_finish(value conn) 
{
  PQfinish(PGconn_val(conn));
  return Val_unit;
}

/* ML type : pgconn_ -> unit */
EXTERNML value pq_reset(value conn) 
{
  PQreset(PGconn_val(conn));
  return Val_unit;
}

/* ML type : pgconn_ -> string -> pgresult_ */
EXTERNML value pq_exec(value conn, value query) 
{
  PGresult* pgres = PQexec(PGconn_val(conn), String_val(query));
  if (pgres == NULL)
    failwith("pq_exec query failed");
  return pgresult_alloc(pgres);
}

/* The function below must agree with the order of the constructors in
   the ML datatype Postgres.pgresultstatus: */

#define Bad_response    0 
#define Command_ok      1 	
#define Copy_in         2 
#define Copy_out        3 
#define Empty_query	4 
#define Fatal_error     5 
#define Nonfatal_error  6 
#define Tuples_ok       7 

/* ML type : pgresult_ -> pgresultstatus */
EXTERNML value pq_resultstatus(value pgresval) 
{
  switch (PQresultStatus(PGresult_val(pgresval))) {
  case PGRES_EMPTY_QUERY:	
    return Atom(Empty_query);
  case PGRES_COMMAND_OK:
    return Atom(Command_ok);
  case PGRES_TUPLES_OK:
    return Atom(Tuples_ok);
  case PGRES_COPY_OUT:
    return Atom(Copy_out);
  case PGRES_COPY_IN:
    return Atom(Copy_in);
  case PGRES_BAD_RESPONSE:
    return Atom(Bad_response);
  case PGRES_NONFATAL_ERROR:
    return Atom(Nonfatal_error);
  case PGRES_FATAL_ERROR:
    return Atom(Tuples_ok);
  default: 
    failwith("mpq:pg_resultstatus: internal error");
  }
}

/* ML type : pgresult_ -> int */
EXTERNML value pq_ntuples(value pgresval) 
{
  return Val_long(PQntuples(PGresult_val(pgresval)));
}

/* ML type : pgresult_ -> int */
EXTERNML value pq_cmdtuples(value pgresval) 
{
  const char* s = PQcmdTuples(PGresult_val(pgresval));
  if (s == NULL)
    failwith("pq_cmdtuples");    
  return Val_long(atoi(s));
}

/* ML type : pgresult_ -> int */
EXTERNML value pq_nfields(value pgresval) 
{
  return Val_long(PQnfields(PGresult_val(pgresval)));
}

/* PostgreSQL does not check that tuple and fields numbers are
   non-negative, and although it appears to check the upper bounds, it
   frequently crashes anyway.  So we have to do it the hard way: */

void checkfbound(PGresult* pgres, int f, char* fcn) 
{
  if (f < 0 || f >= PQnfields(pgres)) { 
    char buf[128];
    sprintf(buf, 
	    "Postgres.%s: illegal field number %d; must be in [0..%d]", 
	    fcn, f, PQnfields(pgres)-1);
    failwith(buf);
  } 
}

void checkbounds(value pgresval, value tupno, value fieldno, char* fcn) 
{
  PGresult* pgres = PGresult_val(pgresval);
  int t = Long_val(tupno);
  int f = Long_val(fieldno);
  checkfbound(pgres, f, fcn);
  if (t < 0 || t >= PQntuples(pgres)) {
    char buf[128];
    sprintf(buf, 
	    "Postgres.%s: illegal tuple number %d; must be in [0..%d]", 
	    fcn, t, PQntuples(pgres)-1);
    failwith(buf);
  }
}

/* ML type : pgresult_ -> int -> string */
EXTERNML value pq_fname(value pgresval, value fieldno) 
{
  checkfbound(PGresult_val(pgresval), Long_val(fieldno), "pq_ftype");
  return copy_string(PQfname(PGresult_val(pgresval), Long_val(fieldno)));
}

/* ML type : pgresult_ -> string -> int */
EXTERNML value pq_fnumber(value pgresval, value fieldname) 
{
  return Val_long(PQfnumber(PGresult_val(pgresval), String_val(fieldname)));
}

/* For now, let us pretend that (32 bit) Oids are (31 bit) ML integers: */

/* ML type : pgresult_ -> int -> int */
EXTERNML value pq_ftype(value pgresval, value fieldno) 
{
  checkfbound(PGresult_val(pgresval), Long_val(fieldno), "pq_ftype");
  return Val_long(PQftype(PGresult_val(pgresval), Long_val(fieldno)));
}

/* ML type : pgresult_ -> int -> int */
EXTERNML value pq_fsize(value pgresval, value fieldno) 
{
  checkfbound(PGresult_val(pgresval), Long_val(fieldno), "pq_ftype");
  return Val_long(PQfsize(PGresult_val(pgresval), Long_val(fieldno)));
}

/* See /usr/local/pgsql/include/postgres.h for the PostgreSQL C types */

/* ML type : pgresult_ -> int -> int -> int */
EXTERNML value pq_getint(value pgresval, value tupno, value fieldno) 
{
  char* v;
  checkbounds(pgresval, tupno, fieldno, "pq_getint");
  v = PQgetvalue(PGresult_val(pgresval), Long_val(tupno), 
		 Long_val(fieldno));
  if (v == NULL)
    failwith("pq_getint");
  return Val_long(atoi(v));
}

/* ML type : pgresult_ -> int -> int -> real */
EXTERNML value pq_getreal(value pgresval, value tupno, value fieldno) 
{
  char* v;
  checkbounds(pgresval, tupno, fieldno, "pq_getreal");
  v = PQgetvalue(PGresult_val(pgresval), Long_val(tupno), 
		 Long_val(fieldno));
  if (v == NULL)
    failwith("pq_getreal");
  return copy_double(atof(v));
}

/* ML type : pgresult_ -> int -> int -> string */
EXTERNML value pq_getstring(value pgresval, value tupno, value fieldno) 
{
  char* v;
  checkbounds(pgresval, tupno, fieldno, "pq_getstring");
  v = PQgetvalue(PGresult_val(pgresval), Long_val(tupno), 
		 Long_val(fieldno));
  if (v == NULL)
    failwith("pq_getstring");
  return copy_string(v);
}

/* ML type : pgresult_ -> int -> int -> bool */
EXTERNML value pq_getbool(value pgresval, value tupno, value fieldno) 
{
  char* v;
  checkbounds(pgresval, tupno, fieldno, "pq_getbool");
  v = PQgetvalue(PGresult_val(pgresval), Long_val(tupno), 
		 Long_val(fieldno));
  if (v == NULL)
    failwith("pq_getbool");
  return Val_bool(!strcmp(v, "t"));
}

/* ML type : pgresult_ -> int -> int -> bool */
EXTERNML value pq_getisnull(value pgresval, value tupno, value fieldno) 
{
  checkbounds(pgresval, tupno, fieldno, "pq_getisnull");
  return Val_bool(PQgetisnull(PGresult_val(pgresval), Long_val(tupno), 
			      Long_val(fieldno)));
}

/* Must be at least 4, but 80 (one line) seems more reasonable: */
#define INITIALSIZE 80

/* ML type : pgconn_ -> string option */
EXTERNML value pq_getline(value conn) 
{
  int bufsize = INITIALSIZE;
  char* buf = (char*)(malloc(bufsize));
  int status;
  value res;

  status = PQgetline(PGconn_val(conn), &buf[0], bufsize);
  if (status != EOF && buf[0] == '\\' && buf[1] == '.' && buf[2] == '\0')
    status = EOF;

  while (status == 1) {		/* Not finished reading line; read more */
    buf = realloc(buf, bufsize * 2);
    status = PQgetline(PGconn_val(conn), &buf[bufsize-1], bufsize+1);
    bufsize *= 2;
  }

  res = Val_stringornull(status == EOF ? NULL : buf);
  free(buf);
  return res;
}

/* ML type : pgconn_ -> string -> unit */
EXTERNML value pq_putline(value conn, value line) 
{
  PQputline(PGconn_val(conn), String_val(line));
  return Val_unit;
}

/* ML type : pgconn_ -> unit */
EXTERNML value pq_endcopy(value conn) 
{
  PQendcopy(PGconn_val(conn));
  return Val_unit;
}
