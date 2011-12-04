-module(tokyo_test).
-export([dict_write/0,dict_read/1,ets_write/0,ets_read/1, tokyo_write/1, tokyo_read/1, tokyot_write/0, tokyot_read/1, btree_iter/1]).

dict_write()->
    D = dict:new(),
    dict_write(D,1000000).

dict_write(D,0)->D;
dict_write(D,N)->dict_write(dict:store(N,N,D),N-1).

dict_read(D)->
    [ dict:find(X,D) || X<-lists:seq(0,1000000) ].

tokyo_write(DB)->
    tokyo_nif:init(),
    B = tokyo_nif:bdbnew(),
    {ok, Cwd} = file:get_cwd(),
    tokyo_nif:bdbopen(B, DB),
    tokyo_write(ok,B,1000000).

tokyo_write(ok,B,0)->B;
tokyo_write(ok,B,N)->tokyo_write(tokyo_nif:bdbput(B,bert:encode(N),bert:encode(N)),B,N-1).

tokyo_read(B)->
    [tokyo_nif:bdbget(B,bert:encode(X)) || X<-lists:seq(0,1000000)],
    tokyo_nif:bdbclose(B).

btree_iter(B) ->
    C = tokyo_nif:tcbdbcurnew(B),
    tokyo_nif:tcbdbcurfirst(C),
    btree_iter(ok, C).

btree_iter(error, C) ->
    error_logger:error_msg("Finnished"),
    tokyo_nif:tcbdbcurdel(C);
btree_iter(ok, C) ->
    K = tokyo_nif:tcbdbcurkey(C),
    V = tokyo_nif:tcbdbcurval(C),
    %%io:format("~i~i~n",[bert:decode(K),bert:decode(V)]),
    case tokyo_nif:tcbdbcurnext(C) of
	ok ->
	    btree_iter(ok, C);
	error ->
	    btree_iter(error, C)
    end.

tokyot_write()->
    tokyo_nif:init(),
    B = tokyo_nif:tdbnew(),
    tokyo_nif:tdbopen(B, "test.tct","TDBOWRITER | TDBOCREAT"),
    tokyot_write(ok,B,1000000).

tokyot_write(ok,B,0)->B;
tokyot_write(ok,B,N)->tokyot_write(tokyo_nif:tdbput3(B,integer_to_list(N),integer_to_list(N)++"\t test"),B,N-1).

tokyot_read(B)->
    [ tokyo_nif:tdbget3(B,integer_to_list(X)) || X<-lists:seq(0,1000000) ]. 

ets_write()->
    E = dets:new(test,[set]),
    [ dets:insert(E,{X,X}) || X<-lists:seq(0,1000000) ],
    E.

ets_read(E)->
    [ dets:lookup(E,X) || X<-lists:seq(0,1000000) ]. 
