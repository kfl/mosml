/* mmysql.c -- Moscow ML interface interface to the mysql library.
   thomassi@dina.kvl.dk 1999-07-06  
   sestoft@dina.kvl.dk 1999-08-07, 2000-05-30 */

#include <stdlib.h>

#ifdef WIN32
#include <windows.h>
#include <stdio.h>
#include <string.h>
#endif

/* Access to the camlrunm/Moscow ML runtime data representation: */

#include <mlvalues.h>
#include <memory.h>
#include <str.h>
#include <fail.h>
#include <alloc.h>

/* Access to Mysql stuff: */

#include "mysql.h"
#include "mysqld_error.h"
#include "errmsg.h"

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

/* A connection dbconn_ is an abstract object

              header with Abstract_tag
              0: MYSQL pointer

   whose component 0 is a pointer to a MYSQL value.  A MySQL database
   must be closed explicitly for its resources to be deallocated.


   A query result dbresult_ should be a finalized object: a triple

              header with Final_tag
              0: finalization function dbresult_finalize
              1: MYSQL_RES pointer
              2: MYSQL_ROW_OFFSET pointer

   whose component 0 is a pointer to the finalizing function
   dbresult_finalize, whose component 1 is a pointer to a MYSQL_RES
   value, and whose component 2 is a pointer to an array of 
   MYSQL_ROW_OFFSETs, used for fast indexing into the result set.  The 
   length of the array equals the number of tuples in the result set.
   The array is needed because we use mysql_row_seek, which seems to 
   take time O(1), for indexing into the resultset.  Using 
   mysql_data_seek was a bad idea, that appeared to take time O(n).
   The finalization function will apply mysql_free_result to component 
   1 and free the component 2 array.
*/

#define DBconn_val(x) ((MYSQL*)(Field(x, 0)))

#define DBresult_val(x)      ((MYSQL_RES*)(Field(x, 1)))
#define DBresultindex_val(x) ((MYSQL_ROW_OFFSET*)(Field(x, 2)))

value dbconn_alloc(MYSQL* conn)
{ 
  value res = alloc(1, Abstract_tag);
  initialize(&Field(res, 0), (value)conn);
  return res;
}

void dbresult_finalize(value dbresval)
{ 
  MYSQL_RES* dbres = DBresult_val(dbresval);
  MYSQL_ROW_OFFSET* index = DBresultindex_val(dbresval);
  if (dbres != NULL) {
    mysql_free_result(dbres);
    DBresult_val(dbresval) = NULL;
    stat_free((char*)index);
    DBresultindex_val(dbresval) = NULL;
  }
}

/* When the dbresult_ value becomes unreachable from the ML process,
   it will be garbage-collected, and dbresult_finalize() will be
   called on the pointer to the MYSQL_RES struct to deallocate it.  */

value dbresult_alloc(MYSQL_RES* dbres) { 
  value res = alloc_final(3, &dbresult_finalize, 1, 10000);
  MYSQL_ROW_OFFSET* index = NULL;
  initialize(&Field(res, 1), (value)dbres);
  if (dbres != NULL) {
    int numrows = mysql_num_rows(dbres);
    if (numrows > 0) {
      int i = 0;
      MYSQL_ROW row;
      index = (MYSQL_ROW_OFFSET*)
	(stat_alloc(sizeof(MYSQL_ROW_OFFSET) * numrows));
      for (i=0; i<numrows; i++) {
	index[i] = mysql_row_tell(dbres);
	mysql_fetch_row(dbres);
      }
    }
  }
  initialize(&Field(res, 2), (value)index);
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

/* ML type : dbconn_ -> string */
EXTERNML value db_db(value conn) 
{
  return copy_string((DBconn_val(conn))->db);
}

/* ML type : dbconn_ -> string */
EXTERNML value db_host(value conn) 
{
  return Val_stringornull((DBconn_val(conn))->host);
}

/* ML type : dbconn_ -> string */
EXTERNML value db_options(value conn) 
{
  return copy_string(""); // FOR NOW
}

/* ML type : dbconn_ -> string */
EXTERNML value db_port(value conn) 
{
  char strport[5];
  sprintf(strport,"%i",(DBconn_val(conn))->port);
  strport[4]='\0';
  return copy_string(strport);
}

/* ML type : dbconn_ -> string */
EXTERNML value db_tty(value conn) 
{
  return copy_string(""); // FIXME: FOR NOW
}

/* ML type : dbconn_ -> string option */
EXTERNML value db_errormessage(value conn) 
{
  char* msg = mysql_error(DBconn_val(conn));
  if (msg == NULL || msg[0] == '\0')
    msg = NULL;
  return Val_stringornull(msg);
}

/* ML type : dbconn_ -> unit */
EXTERNML value db_finish(value conn) 
{
  mysql_close(DBconn_val(conn));
  return Val_unit;
}

#if defined(WIN32) && (MYSQL_VERSION_ID < 32200)
int mysql_ping(MYSQL* dummy)
{
  return 1;
}
#endif

/* ML type : dbconn_ -> unit */
EXTERNML value db_reset(value conn) 
{
  mysql_ping(DBconn_val(conn));
  return Val_unit;
}

/* ML type : dbconn_ -> unit */
EXTERNML value db_status(value conn) 
{
  mysql_ping(DBconn_val(conn));
  return Val_bool(mysql_ping(DBconn_val(conn)));
}


/* ML type : dbresult_ -> int */
EXTERNML value db_ntuples(value dbresval) 
{
  MYSQL_RES* dbres = DBresult_val(dbresval);
  long ntuples;
  if (dbres == NULL)
    return Val_long(0);
  /* NB: Cast from long long int to long int: */
  ntuples = (long)(mysql_num_rows(dbres));
  return Val_long(ntuples);
}

/* ML type : dbresult_ -> int */
EXTERNML value db_cmdtuples(value conn) 
{
  /* NB: Cast from long long int to long int: */
  long cmdtuples = (long)(mysql_affected_rows(DBconn_val(conn))); 
  return Val_long(cmdtuples);
}

/* ML type : dbresult_ -> int */
EXTERNML value db_nfields(value dbresval) 
{
  MYSQL_RES* dbres = DBresult_val(dbresval);
  if (dbres == NULL)
    return Val_long(0);
  /* NB: Cast from int to long int: */
  return Val_long((long)(mysql_num_fields(dbres)));
}

/* MySQL does not check that tuple and fields numbers and
   non-negative, and although it appears to check the upper bounds, it
   frequently crashes anyway.  So we have to do it the hard way: */

void checkfbound(MYSQL_RES* dbres, int f, char* fcn) 
{
  if (dbres == NULL)
    failwith("Mysql: non-select dbresult");
  if (f < 0 || f >= (int)mysql_num_fields(dbres)) { 
    char buf[128];
    sprintf(buf, 
            "Mysql.%s: illegal field number %d; must be in [0..%d]", 
            fcn, f, mysql_num_fields(dbres)-1);
    failwith(buf);
  } 
}

void checkbounds(value dbresval, value tupno, value fieldno, char* fcn) 
{
  MYSQL_RES* dbres = DBresult_val(dbresval);
  int t = Long_val(tupno);
  int f = Long_val(fieldno);
  checkfbound(dbres, f, fcn);
  if (t < 0 || t >= ((int)mysql_num_rows(dbres))) {
    char buf[128];
    sprintf(buf, 
            "Mysql.%s: illegal tuple number %d; must be in [0..%d]", 
            fcn, t, mysql_num_rows(dbres)-1);
    failwith(buf);
  }
}

/* ML type : dbresult_ -> int -> string */
EXTERNML value db_fname(value dbresval, value fieldno) 
{
  MYSQL_FIELD *fields;
  checkfbound(DBresult_val(dbresval), Long_val(fieldno), "db_fname");
  fields=mysql_fetch_fields(DBresult_val(dbresval));
  return copy_string(fields[Long_val(fieldno)].name);
}

/* ML type : dbresult_ -> string -> int */
EXTERNML value db_fnumber(value dbresval, value fieldnameval) 
{
  char* fieldname = String_val(fieldnameval);
  MYSQL_RES* dbres = DBresult_val(dbresval);
  if (dbres == NULL) 
    return Val_long(-1);        /* No such field name */
  {
    unsigned int num_fields = mysql_num_fields(dbres);
    MYSQL_FIELD *fields = mysql_fetch_fields(dbres);
    unsigned int i;
    for(i = 0; i < num_fields; i++) {
      if(strcmp(fields[i].name, fieldname)==0) 
        return Val_long(i);
    }
  }
  return Val_long(-1);          /* No such field name */
}

/* ML type : dbresult_ -> int -> int */
EXTERNML value db_ftype(value dbresval, value fieldno) 
{
  /* Fetch field information */
  // NB.   fetch_field direct doesn't work. (mysql is broken)

  //  NB!: The numbers below need to correspond to the
  //       numbers in Mysql.sml

  MYSQL_FIELD *fields;
  checkfbound(DBresult_val(dbresval), Long_val(fieldno), "db_ftype");
  fields=mysql_fetch_fields(DBresult_val(dbresval));

  switch(fields[Long_val(fieldno)].type) {
  case FIELD_TYPE_DECIMAL:
    return Val_long(0);
  case FIELD_TYPE_TINY:
    return Val_long(1);
  case FIELD_TYPE_SHORT:
    return Val_long(2);
  case FIELD_TYPE_LONG:
    return Val_long(3);
  case FIELD_TYPE_FLOAT:
    return Val_long(4);
  case FIELD_TYPE_DOUBLE:
    return Val_long(5);
  case FIELD_TYPE_NULL:
    return Val_long(6);
  case FIELD_TYPE_TIMESTAMP:
    return Val_long(7);
  case FIELD_TYPE_LONGLONG:
    return Val_long(8);
  case FIELD_TYPE_INT24:
    return Val_long(9);
  case FIELD_TYPE_DATE:
    return Val_long(10);
  case FIELD_TYPE_TIME:
    return Val_long(11);
  case FIELD_TYPE_DATETIME:
    return Val_long(12);
  case FIELD_TYPE_YEAR:
    return Val_long(13);
  case FIELD_TYPE_NEWDATE:
    return Val_long(14);
  case FIELD_TYPE_ENUM:
    return Val_long(15);
  case FIELD_TYPE_SET:
    return Val_long(16);
  case FIELD_TYPE_TINY_BLOB:
    return Val_long(17);
  case FIELD_TYPE_MEDIUM_BLOB:
    return Val_long(18);
  case FIELD_TYPE_LONG_BLOB:
    return Val_long(19);
  case FIELD_TYPE_BLOB:
    return Val_long(20);
  case FIELD_TYPE_VAR_STRING:
    return Val_long(21);
  case FIELD_TYPE_STRING:
    return Val_long(22);

    // broken by design: FIELD_TYPE_CHAR=FIELD_TYPE_TINY,
    // really shouldn't be, as it's to different concepts
    //  case FIELD_TYPE_CHAR:
    // return copy_string(17);
  }

  //default type
  return Val_long(-1);
}

/* See /usr/local/include/mysql/mysql.h for the Mysql C types */

MYSQL_ROW seekandgetrow(value dbresval, int n) {
  /* mysql_row_seek seems to take time O(1), mysql_data_seek takes O(n) */
  mysql_row_seek(DBresult_val(dbresval), 
		 DBresultindex_val(dbresval)[n]);
  return mysql_fetch_row(DBresult_val(dbresval));
}

/* ML type : dbresult_ -> int -> int -> int */
EXTERNML value db_getint(value dbresval, value tupno, value fieldno) 
{
  MYSQL_ROW row;
  checkbounds(dbresval, tupno, fieldno, "db_getint");
  row = seekandgetrow(dbresval, Long_val(tupno));
  if (row == NULL)
    failwith("db_getint 2");
  return Val_long(atoi(row[Long_val(fieldno)])); 
}

/* ML type : dbresult_ -> int -> int -> real */
EXTERNML value db_getreal(value dbresval, value tupno, value fieldno) 
{
  MYSQL_ROW row;
  checkbounds(dbresval, tupno, fieldno, "db_getreal");
  row = seekandgetrow(dbresval, Long_val(tupno));
  if (row == NULL)
    failwith("db_getreal");
  return copy_double(atof(row[Long_val(fieldno)]));
}

/* ML type : dbresult_ -> int -> int -> string */
EXTERNML value db_getstring(value dbresval, value tupno, value fieldno) 
{
  MYSQL_ROW row;
  checkbounds(dbresval, tupno, fieldno, "db_getstring");
  row = seekandgetrow(dbresval, Long_val(tupno));
  if (row == NULL)
    failwith("db_getint");
  return copy_string(row[Long_val(fieldno)]);
}

/* ML type : dbresult_ -> int -> int -> bool */
EXTERNML value db_getbool(value dbresval, value tupno, value fieldno) 
{
  MYSQL_ROW row;
  checkbounds(dbresval, tupno, fieldno, "db_getbool");
  row = seekandgetrow(dbresval, Long_val(tupno));
  if (row == NULL)
    failwith("db_getbool");
  return Val_bool(!strcmp(row[Long_val(fieldno)], "t"));
}

/* ML type : dbresult_ -> int -> int -> bool */
EXTERNML value db_getisnull(value dbresval, value tupno, value fieldno) 
{
  MYSQL_ROW row;
  checkbounds(dbresval, tupno, fieldno, "db_getisnull");
  row = seekandgetrow(dbresval, Long_val(tupno));
  if (row == NULL)
    failwith("db_getisnull");
  return Val_bool(row[Long_val(fieldno)]==NULL); 
}

/* ML type : 6-element record -> dbconn_ */
EXTERNML value mysql_setdb(value args) 
{
  char* dbhost    = StringOrNull_val(Field(args, 0));
  char* dbname    = StringOrNull_val(Field(args, 1));
  char* dboptions = StringOrNull_val(Field(args, 2));
  unsigned dbport = (unsigned)(Long_val(Field(args, 3)));
  char* dbpwd     = StringOrNull_val(Field(args, 4));
  char* dbtty     = StringOrNull_val(Field(args, 5));
  char* dbuser    = StringOrNull_val(Field(args, 6));
  
#if MYSQL_VERSION_ID >= 32200  
  MYSQL *mysql = mysql_init(NULL);
  if (mysql==NULL) {
    failwith("mysql_init failed - out of memory");
  } else {
    MYSQL* newmysql = mysql_real_connect(mysql, dbhost, dbuser, dbpwd,
      dbname, dbport, NULL, 0);
    if(newmysql==NULL) {
      failwith(mysql_error(mysql));
    } else {
      return (value)(dbconn_alloc(newmysql));
    }
  }
#else
  MYSQL* mysql = mysql_real_connect(NULL, dbhost, dbuser, dbpwd,
    dbport, NULL, 0);
  if (mysql==NULL) {
    failwith("Could not connect");
  } else {
    if (!mysql_select_db(mysql, dbname))
      failwith(mysql_error(mysql));
    return (value)(dbconn_alloc(mysql));
  }
#endif
}

/* ML type : dbconn_ -> string -> dbresult_ */
EXTERNML value db_exec(value conn, value query) 
{
  if (mysql_real_query(DBconn_val(conn), String_val(query), string_length(query)))
    failwith("db_exec query failed"); 
  else {
    MYSQL_RES *dbres = mysql_store_result(DBconn_val(conn));
    return dbresult_alloc(dbres); 
  }
}

/* The function below must agree with the order of the constructors in
   the ML datatype Mysql.dbresultstatus: */

#define Bad_response    0 
#define Command_ok      1 	
#define Copy_in         2 
#define Copy_out        3 
#define Empty_query	4 
#define Fatal_error     5 
#define Nonfatal_error  6 
#define Tuples_ok       7 

/* ML type : dbconn_ -> dbresultstatus */
EXTERNML value db_resultstatus(value conn) {
  MYSQL *mysql = DBconn_val(conn);
  switch (mysql_errno(mysql)) {
  case ER_EMPTY_QUERY:  
    return Atom(Empty_query);
  case 0:                       /* No error */
    {
      /* If mysql_num_fields==0, query was a command */
      if (mysql_num_fields(mysql) == 0)
        return Atom(Command_ok);
      else
        return Atom(Tuples_ok);
    }
  default: 
    return Atom(Nonfatal_error);
  }
}
