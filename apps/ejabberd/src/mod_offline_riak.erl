-module(mod_offline_riak).

-compile([export_all]).

-include("mod_offline.hrl").

-define(OFFLINE_BUCKET, <<"offline_msg">>).

%% TODO: adjust for binary-/string-only versions
-type string_type() :: binary() | string().
-type user() :: {string_type(), string_type()}.
-type time() :: {integer(), integer(), integer()}.

create_table() ->
    ok.

clear_table() ->
    ejabberd_riak:delete(?OFFLINE_BUCKET).

-spec store_offline_messages(user(), list(), infinity | integer())
    -> ok | {error, any()}.
store_offline_messages(US, Msgs, MaxOfflineMsgs) ->
    {ok, OldMsgs} = get_messages(US),
    NewMsgs = lists:ukeysort(#offline_msg.timestamp,
        OldMsgs ++ Msgs),
    Len = length(NewMsgs),
    if
        MaxOfflineMsgs =/= infinity andalso Len > MaxOfflineMsgs ->
            mod_offline:discard_warn_sender(Msgs),
            ok;
        true ->
            ejabberd_riak:set(?OFFLINE_BUCKET, US, NewMsgs)
    end.

%% Opts may include:
%% - dirty | {dirty, true | false} - the read will (or will not) be done
%%   using mnesia:dirty_read instead of doing it inside a transaction
%%   context and the return value is the dirty_read's raw return value,
%%   also no messages are removed from the database
-spec load_offline_messages(user(), Opts :: list())
    ->  Messages :: list()
    |   {ok, Messages :: list()}
    |   {error, any()}.
load_offline_messages(US, Opts) ->
    case get_messages(US) of
        {ok, Msgs} ->
            case proplists:get_value(dirty, Opts, false) of
                true ->
                    Msgs;
                false ->
                    ejabberd_riak:delete(?OFFLINE_BUCKET, US),
                    {ok, Msgs}
            end;
        Error ->
            Error
    end.

-spec remove_expired_messages(time()) -> ok | {error, any()}.
remove_expired_messages(TimeStamp) ->
    {error, not_implemented_yet}.
    %mnesia:foldl(
        %fun(Rec, _Acc) ->
            %case Rec#offline_msg.expire of
                %never ->
                    %ok;
                %TS ->
                    %if
                        %TS < TimeStamp ->
                            %mnesia:delete_object(Rec);
                        %true ->
                            %ok
                    %end
            %end
        %end, ok, offline_msg)

-spec remove_old_messages(time()) -> ok | {error, any()}.
remove_old_messages(TimeStamp) ->
    {error, not_implemented_yet}.
    %F = fun() ->
        %mnesia:write_lock_table(offline_msg),
        %mnesia:foldl(
            %fun(#offline_msg{timestamp = TS} = Rec, _Acc)
                %when TS < TimeStamp ->
                    %mnesia:delete_object(Rec);
                %(_Rec, _Acc) -> ok
            %end, ok, offline_msg)
    %end,
    %transaction_no_result(F).

-spec remove_user(user()) -> ok | {error, any()}.
remove_user(US) ->
    ejabberd_riak:delete(?OFFLINE_BUCKET, US).

%% Supported Opts:
%% - write_lock - the whole table is write locked at the start of the
%%   transaction
-spec delete_offline_messages(list(), list()) -> ok | {error, any()}.
delete_offline_messages(Messages, Opts) ->
    {error, not_implemented_yet}.
    %F = fun() ->
        %case proplists:get_value(write_lock, Opts, false) of
            %true ->
                %mnesia:write_lock_table(offline_msg);
            %false ->
                %ok
        %end,
        %lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages)
    %end,
    %transaction_no_result(F).

%%% Helpers

get_messages(US) ->
    case ejabberd_riak:get(?OFFLINE_BUCKET, US) of
        {ok, Msgs} ->
            {ok, Msgs};
        {error, notfound} ->
            {ok, []};
        Error ->
            Error
    end.
