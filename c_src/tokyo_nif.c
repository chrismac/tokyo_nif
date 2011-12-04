#include <tcutil.h>
#include <tcadb.h>
#include <tctdb.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include "erl_nif.h"

static ERL_NIF_TERM nif_bdbnew(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
{
  unsigned long bdb;
  
  bdb = (unsigned long)tcbdbnew();
  
  return enif_make_ulong(env, bdb);
}

static ERL_NIF_TERM nif_bdbopen(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*                                ERL_NIF_TERM bdb_tm,
 *                              ERL_NIF_TERM filename_tm)
 */
{
  char filename[1024];
  unsigned long bdb;
  int ecode;
  
  enif_get_ulong(env, argv[0], &bdb);
  enif_get_string(env, argv[1], filename, sizeof(argv[1]), ERL_NIF_LATIN1);
  if (tcbdbopen((TCBDB*)bdb,filename, BDBOWRITER | BDBOCREAT )) {
    return enif_make_atom(env, "ok");
  } else {
    return enif_make_atom(env, tcbdberrmsg(ecode));
  }
}

static ERL_NIF_TERM nif_tclistnew(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
{
  unsigned long list;
  
  list = (unsigned long)tclistnew();
  
  return enif_make_ulong(env, list);
}

static ERL_NIF_TERM nif_bdbclose(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*                                 ERL_NIF_TERM bdb_tm)*/
{
  unsigned long bdb;
  
  enif_get_ulong(env, argv[0], &bdb);
  if (tcbdbclose((TCBDB*)bdb)) {
    return enif_make_atom(env, "ok");
  } else {
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_bdbput(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*                               ERL_NIF_TERM bdb_tm,
 *                             ERL_NIF_TERM kbin_tm,
 *                             ERL_NIF_TERM vbin_tm)
 */
{
  unsigned long bdb;
  ErlNifBinary kbin, vbin;
  
  enif_get_ulong(env, argv[0], &bdb);
  enif_inspect_binary(env, argv[1], &kbin);
  enif_inspect_binary(env, argv[2], &vbin);
    
  if (tcbdbput((TCBDB*)bdb, kbin.data, kbin.size, vbin.data, vbin.size)) {
    enif_release_binary(env, &kbin);
    enif_release_binary(env, &vbin);
    return enif_make_atom(env, "ok");
  } else {
    enif_release_binary(env, &kbin);
    enif_release_binary(env, &vbin);
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_bdbputdup(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*                               ERL_NIF_TERM bdb_tm,
 *                             ERL_NIF_TERM kbin_tm,
 *                             ERL_NIF_TERM vbin_tm)
 */
{
  unsigned long bdb;
  ErlNifBinary kbin, vbin;
  
  enif_get_ulong(env, argv[0], &bdb);
  enif_inspect_binary(env, argv[1], &kbin);
  enif_inspect_binary(env, argv[2], &vbin);
    
  if (tcbdbputdup((TCBDB*)bdb, kbin.data, kbin.size, vbin.data, vbin.size)) {
    enif_release_binary(env, &kbin);
    enif_release_binary(env, &vbin);
    return enif_make_atom(env, "ok");
  } else {
    enif_release_binary(env, &kbin);
    enif_release_binary(env, &vbin);
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_bdbget(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*                                ERL_NIF_TERM bdb_tm,
 *                              ERL_NIF_TERM kbin_tm)
 */
{
  unsigned long bdb;
  char *vstr;
  ErlNifBinary kbin, vbin, vbin_tm;
  int vsize;

  enif_get_ulong(env, argv[0], &bdb);
  enif_inspect_binary(env, argv[1], &kbin);
  if ((vstr = tcbdbget((TCBDB*)bdb, kbin.data, kbin.size, &vsize))) {
    enif_alloc_binary(env, vsize, &vbin);
    memcpy(vbin.data, vstr, vsize);
    tcfree(vstr);
    return enif_make_binary(env, &vbin);
  } else {
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_bdbget4(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*                                ERL_NIF_TERM bdb_tm,
 *                              ERL_NIF_TERM kbin_tm)
 */
{
  unsigned long bdb;
  TCLIST *list;
  ErlNifBinary kbin;

  enif_get_ulong(env, argv[0], &bdb);
  enif_inspect_binary(env, argv[1], &kbin);
  if ((list = tcbdbget4((TCBDB*)bdb, kbin.data, kbin.size))) {
    return enif_make_ulong(env, (unsigned long)list);
  } else {
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_tclistpop(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				  ERL_NIF_TERM tcl_tm)*/
{
  unsigned long tcl;
  ErlNifBinary vbin;
  int vsize;
  char *vstr;

  enif_get_ulong(env, argv[0], &tcl);
  if ((vstr = tclistpop((TCLIST*)tcl, &vsize))) {
    enif_alloc_binary(env, vsize, &vbin);
    memcpy(vbin.data, vstr, vsize);
    tcfree(vstr);
    return enif_make_binary(env, &vbin);
  } else {
    free((TCLIST*)tcl);
    return enif_make_atom(env, "empty");
  }
}

static ERL_NIF_TERM nif_tclistshift(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				  ERL_NIF_TERM tcl_tm)*/
{
  unsigned long tcl;
  ErlNifBinary vbin;
  int vsize;
  char *vstr;

  enif_get_ulong(env, argv[0], &tcl);
  if ((vstr = tclistshift((TCLIST*)tcl, &vsize))) {
    enif_alloc_binary(env, vsize, &vbin);
    memcpy(vbin.data, vstr, vsize);
    tcfree(vstr);
    return enif_make_binary(env, &vbin);
  } else {
    free((TCLIST*)tcl);
    return enif_make_atom(env, "empty");
  }
}


static ERL_NIF_TERM nif_tdbput3(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*                                ERL_NIF_TERM tdb_tm,
 *                              ERL_NIF_TERM kstr_tm,
 *                              ERL_NIF_TERM vstr_tm)
 */
{
  unsigned long tdb;
  char kstr[1024];
  char vstr[1024];
  int ecode;
  
  enif_get_ulong(env, argv[0], &tdb);
  enif_get_string(env, argv[1], kstr, sizeof(argv[1]), ERL_NIF_LATIN1);
  enif_get_string(env, argv[2], vstr, sizeof(argv[2]), ERL_NIF_LATIN1);
  if (tctdbput3((TCTDB*)tdb, kstr, vstr)) {
    return enif_make_atom(env, "ok");
  } else {
    ecode = tctdbecode((TCTDB*)tdb);
    return enif_make_atom(env, tctdberrmsg(ecode));
  }
}

static ERL_NIF_TERM nif_tdbget3(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*                                ERL_NIF_TERM tdb_tm,
 *				  ERL_NIF_TERM kstr_tm)
*/
{
  unsigned long tdb;
  char kstr[1024];
  char *vstr;
  ERL_NIF_TERM vstr_tm;

  enif_get_ulong(env, argv[0], &tdb);
  enif_get_string(env, argv[1], kstr, sizeof(argv[1]), ERL_NIF_LATIN1);
 
  if ((vstr = tctdbget3((TCTDB*)tdb, kstr))) {
    vstr_tm = enif_make_string(env, vstr, ERL_NIF_LATIN1);
    free(vstr);
    return enif_make_tuple(env, 2, enif_make_atom(env, "value"), vstr_tm);
  } else {
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_tclistsort(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				  ERL_NIF_TERM tcl_tm)*/
{
  unsigned long tcl;

  enif_get_ulong(env, argv[0], &tcl);
  tclistsort((TCLIST*)tcl);
  enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_tcbdbcurnew(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				    ERL_NIF_TERM bdb_tm)*/
{
  unsigned long bdb, cur;

  enif_get_ulong(env, argv[0], &bdb);
  cur = (unsigned long)tcbdbcurnew((TCBDB*)bdb);
  
  return enif_make_ulong(env, cur);
}

static ERL_NIF_TERM nif_tcbdbcurfirst(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				    ERL_NIF_TERM cur_tm)*/
{
  unsigned long cur;

  enif_get_ulong(env, argv[0], &cur);
  if (((unsigned long)tcbdbcurfirst((BDBCUR*)cur))) {
    return enif_make_atom(env, "ok");
  } else {
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_tcbdbcurdel(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				    ERL_NIF_TERM cur_tm)*/
{
  unsigned long cur;

  enif_get_ulong(env, argv[0], &cur);
  tcbdbcurdel((BDBCUR*)cur);
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_tcbdbcurnext(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				    ERL_NIF_TERM cur_tm)*/
{
  unsigned long cur;

  enif_get_ulong(env, argv[0], &cur);
  if ((unsigned int)tcbdbcurnext((BDBCUR*)cur)) {
    return enif_make_atom(env, "ok");
  } else {
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_tcbdbcurrec(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				    ERL_NIF_TERM cur_tm)*/
{
  unsigned long cur;
  int ksize, vsize;
  ErlNifBinary kbin, vbin;
  TCXSTR *kstr = tcxstrnew();
  TCXSTR *vstr = tcxstrnew();
  
  enif_get_ulong(env, argv[0], &cur);
  if ((tcbdbcurrec((BDBCUR*)cur, kstr, vstr))) {
    ksize = tcxstrsize(kstr);
    vsize = tcxstrsize(vstr);
    enif_alloc_binary(env, ksize, &kbin);
    enif_alloc_binary(env, vsize, &vbin);
    memcpy(kbin.data, kstr, ksize);
    memcpy(vbin.data, vstr, vsize);
    return enif_make_tuple(env, 2, (enif_make_binary(env, &kbin)), enif_make_binary(env, &vbin));
  } else {
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_tcbdbcurkey(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				    ERL_NIF_TERM cur_tm)*/
{
  unsigned long cur;
  char *key;
  ErlNifBinary kbin;
  int ksize;

  enif_get_ulong(env, argv[0], &cur);
  if ((key = tcbdbcurkey((BDBCUR*)cur, &ksize))) {
    enif_alloc_binary(env, ksize, &kbin);
    memcpy(kbin.data, key, ksize);
    return enif_make_binary(env, &kbin);
  } else {
    return enif_make_atom(env, "error");
  }
}

static ERL_NIF_TERM nif_tcbdbcurval(ErlNifEnv* env,
				    int argc,
				    const ERL_NIF_TERM argv[])
/*				    ERL_NIF_TERM cur_tm)*/
{
  unsigned long cur;
  char *val;
  ErlNifBinary vbin;
  int vsize;

  enif_get_ulong(env, argv[0], &cur);
  if ((val = tcbdbcurval((BDBCUR*)cur, &vsize))) {
    enif_alloc_binary(env, vsize, &vbin);
    memcpy(vbin.data, val, vsize);
    return enif_make_binary(env, &vbin);
  } else {
    return enif_make_atom(env, "error");
  }
}


static ErlNifFunc nif_funcs[] =
{
  {"bdbnew", 0, nif_bdbnew},
  {"bdbopen", 2, nif_bdbopen},
  {"tclistnew", 0, nif_tclistnew},
  {"bdbclose", 1, nif_bdbclose},
  {"bdbput", 3, nif_bdbput},
  {"bdbputdup", 3, nif_bdbputdup},
  {"bdbget", 2, nif_bdbget},
  {"bdbget4", 2, nif_bdbget4},
  {"tclistpop", 1, nif_tclistpop},
  {"tclistshift", 1, nif_tclistshift},
  {"tclistsort", 1, nif_tclistsort},
  {"tcbdbcurnew", 1, nif_tcbdbcurnew},
  {"tcbdbcurfirst", 1, nif_tcbdbcurfirst},
  {"tcbdbcurdel", 1, nif_tcbdbcurdel},
  {"tcbdbcurnext", 1, nif_tcbdbcurnext},
  {"tcbdbcurrec", 1, nif_tcbdbcurrec},
  {"tcbdbcurkey", 1, nif_tcbdbcurkey},
  {"tcbdbcurval", 1, nif_tcbdbcurval},
};

ERL_NIF_INIT(tokyo_nif,nif_funcs,NULL,NULL,NULL,NULL)
