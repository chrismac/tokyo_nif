-module(tokyo_nif).

-export([init/0,
	bdbnew/0,
	bdbopen/2,
	tclistnew/0,
	bdbclose/1,
	bdbput/3,
	bdbputdup/3,
	bdbget/2,
	bdbget4/2,
	tclistpop/1,
	tclistshift/1,
	tclistsort/1,
	tcbdbcurnew/1,
	tcbdbcurfirst/1,
	tcbdbcurdel/1,
	tcbdbcurnext/1,
	tcbdbcurrec/1,
	tcbdbcurkey/1,
	tcbdbcurval/1
        ]).
        
init() ->
  erlang:load_nif("../ebin/tokyo_nif", 0).

bdbnew() ->
  {error, nif_library_not_loaded}.

bdbopen(_Bdb, _Name) ->
  {error, nif_library_not_loaded}.

tclistnew() ->
  {error, nif_library_not_loaded}.

bdbclose(_Adb) ->
  {error, nif_library_not_loaded}.

bdbput(_Adb, _KeyStr, _ValueStr) ->
  {error, nif_library_not_loaded}.

bdbputdup(_Adb, _KeyStr, _ValueStr) ->
  {error, nif_library_not_loaded}.

bdbget(_Adb, _KeyStr) ->
  {error, nif_library_not_loaded}.

bdbget4(_Adb, _KeyStr) ->
  {error, nif_library_not_loaded}.

tclistpop(_Tcl) ->
  {error, nif_library_not_loaded}.

tclistshift(_Tcl) ->
  {error, nif_library_not_loaded}.

tclistsort(_Tcl) ->
  {error, nif_library_not_loaded}.

tcbdbcurnew(_Bdb) ->
  {error, nif_library_not_loaded}.

tcbdbcurfirst(_Cur) ->
  {error, nif_library_not_loaded}.

tcbdbcurdel(_Cur) ->
  {error, nif_library_not_loaded}.

tcbdbcurnext(_Cur) ->
  {error, nif_library_not_loaded}.

tcbdbcurrec(_Cur) ->
  {error, nif_library_not_loaded}.

tcbdbcurkey(_Cur) ->
  {error, nif_library_not_loaded}.

tcbdbcurval(_Cur) ->
  {error, nif_library_not_loaded}.
