-module(ejabberd_riak).

%% Public interface
-export([start_link/1,
         get/2,
         set/3,
         delete/2,
         list_keys/1,
         mapred_bucket/2]).

-include("ejabberd.hrl").

start_link(WorkerId) ->
    {Host, Port} = {get_host(), get_port()},
    {ok, Pid} = connect(Host, Port),
    ejabberd_riak_sup:register(WorkerId, Pid),
    {ok, Pid}.

get(Bucket, Key) ->
    case get_obj(Bucket, Key) of
        {ok, Obj} ->
            {ok, binary_to_term(riakc_obj:get_value(Obj))};
        Error ->
            Error
    end.

-spec get_obj(binary(), term()) -> {ok, term()} | {error, any()}.
get_obj(Bucket, Key) ->
    BinaryKey = term_to_binary(Key),
    riakc_pb_socket:get(get_worker(), Bucket, BinaryKey).

-spec set(binary(), term(), term()) -> ok | {error, any()}.
set(Bucket, Key, Value) ->
    BinaryKey = term_to_binary(Key),
    BinaryValue = term_to_binary(Value),
    case get_obj(Bucket, Key) of
        {ok, Obj} ->
            NewObj = riakc_obj:update_value(Obj, BinaryValue),
            riakc_pb_socket:put(get_worker(), NewObj);
        {error, notfound} ->
            NewObj = riakc_obj:new(Bucket, BinaryKey, BinaryValue),
            riakc_pb_socket:put(get_worker(), NewObj);
        Else ->
            Else
    end.

-spec delete(binary(), term()) -> ok | {error, any()}.
delete(Bucket, Key) ->
    riakc_pb_socket:delete(get_worker(), Bucket, term_to_binary(Key)).

-spec list_keys(binary()) -> {ok, [binary()]} | {error, any()}.
list_keys(Bucket) ->
    case riakc_pb_socket:list_keys(get_worker(), Bucket) of
        {ok, BinKeys} ->
            lists:map(fun erlang:binary_to_term/1, BinKeys).
        Error ->
            Error
    end.

-spec mapred_bucket(binary(), list()) -> any().
mapred_bucket(Bucket, Query) ->
    riakc_pb_socket:mapred_bucket(get_worker(), Bucket, Query).

get_host() ->
    get_config(riak_host, "localhost").

get_port() ->
    get_config(riak_port, 8087).

get_timeout() ->
    get_config(riak_initial_connection_timeout, 10).

get_config(Key, Default) ->
    case ejabberd_config:get_local_config() of
        undefined ->
            Default;
        Value ->
            Value
    end.
