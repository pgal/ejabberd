-module(mod_offline_riak).

-compile([export_all]).

-include("mod_offline.hrl").

%% TODO: adjust for binary-/string-only versions
-type string_type() :: binary() | string().
-type user() :: {string_type(), string_type()}.
-type time() :: {integer(), integer(), integer()}.

create_table() ->
    ok.

-spec store_offline_messages(user(), list(), infinity | integer())
    -> ok | {error, any()}.
store_offline_messages(US, Msgs, MaxOfflineMsgs) ->
    Len = length(Msgs),
    F = fun() ->
        %% Only count messages if needed:
        Count =
            if MaxOfflineMsgs =/= infinity ->
                Len + p1_mnesia:count_records(
                    offline_msg, 
                    #offline_msg{us=US, _='_'});
            true -> 
                0
        end,
        if
            Count > MaxOfflineMsgs ->
                mod_offline:discard_warn_sender(Msgs);
            true ->
                if
                    Len >= ?OFFLINE_TABLE_LOCK_THRESHOLD ->
                        mnesia:write_lock_table(offline_msg);
                    true ->
                        ok
                end,
                lists:foreach(fun(M) -> mnesia:write(M) end, Msgs)
        end
    end,
    transaction_no_result(F).

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
    case proplists:get_value(dirty, Opts, false) of
        true ->
            mnesia:dirty_read({offline_msg, US});
        false ->
            F = fun() ->
                Rs = mnesia:wread({offline_msg, US}),
                mnesia:delete({offline_msg, US}),
                Rs
            end,
            case mnesia:transaction(F) of
                {atomic, Messages} ->
                    {ok, Messages};
                Error ->
                    {error, Error}
            end
    end.

-spec remove_expired_messages(time()) -> ok | {error, any()}.
remove_expired_messages(TimeStamp) ->
    F = fun() ->
        mnesia:write_lock_table(offline_msg),
        mnesia:foldl(
            fun(Rec, _Acc) ->
                case Rec#offline_msg.expire of
                    never ->
                        ok;
                    TS ->
                        if
                            TS < TimeStamp ->
                                mnesia:delete_object(Rec);
                            true ->
                                ok
                        end
                end
            end, ok, offline_msg)
    end,
    transaction_no_result(F).

-spec remove_old_messages(time()) -> ok | {error, any()}.
remove_old_messages(TimeStamp) ->
    F = fun() ->
        mnesia:write_lock_table(offline_msg),
        mnesia:foldl(
            fun(#offline_msg{timestamp = TS} = Rec, _Acc)
                when TS < TimeStamp ->
                    mnesia:delete_object(Rec);
                (_Rec, _Acc) -> ok
            end, ok, offline_msg)
    end,
    transaction_no_result(F).

-spec remove_user(user()) -> ok | {error, any()}.
remove_user(US) ->
    F = fun() ->
        mnesia:delete({offline_msg, US})
    end,
    transaction_no_result(F).

%% Supported Opts:
%% - write_lock - the whole table is write locked at the start of the
%%   transaction
-spec delete_offline_messages(list(), list()) -> ok | {error, any()}.
delete_offline_messages(Messages, Opts) ->
    F = fun() ->
        case proplists:get_value(write_lock, Opts, false) of
            true ->
                mnesia:write_lock_table(offline_msg);
            false ->
                ok
        end,
        lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages)
    end,
    transaction_no_result(F).

%%% Helpers

transaction_no_result(F) ->
    case mnesia:transaction(F) of
        {atomic, _} ->
            ok;
        Error ->
            {error, Error}
    end.

update_table() ->
    Fields = record_info(fields, offline_msg),
    case mnesia:table_info(offline_msg, attributes) of
        Fields ->
            ok;
        [user, timestamp, expire, from, to, packet] ->
            ?INFO_MSG("Converting offline_msg table from "
                "{user, timestamp, expire, from, to, packet} format", []),
            Host = ?MYNAME,
            {atomic, ok} = mnesia:create_table(
                mod_offline_tmp_table,
                [{disc_only_copies, [node()]},
                    {type, bag},
                    {local_content, true},
                    {record_name, offline_msg},
                    {attributes, record_info(fields, offline_msg)}]),
            mnesia:transform_table(offline_msg, ignore, Fields),
            F1 = fun() ->
                    mnesia:write_lock_table(mod_offline_tmp_table),
                    mnesia:foldl(
                        fun(#offline_msg{us = U} = R, _) ->
                                mnesia:dirty_write(
                                    mod_offline_tmp_table,
                                    R#offline_msg{us = {U, Host}})
                        end, ok, offline_msg)
            end,
            mnesia:transaction(F1),
            mnesia:clear_table(offline_msg),
            F2 = fun() ->
                    mnesia:write_lock_table(offline_msg),
                    mnesia:foldl(
                        fun(R, _) ->
                                mnesia:dirty_write(R)
                        end, ok, mod_offline_tmp_table)
            end,
            mnesia:transaction(F2),
            mnesia:delete_table(mod_offline_tmp_table);
        [user, timestamp, from, to, packet] ->
            ?INFO_MSG("Converting offline_msg table from "
                "{user, timestamp, from, to, packet} format", []),
            Host = ?MYNAME,
            {atomic, ok} = mnesia:create_table(
                mod_offline_tmp_table,
                [{disc_only_copies, [node()]},
                    {type, bag},
                    {local_content, true},
                    {record_name, offline_msg},
                    {attributes, record_info(fields, offline_msg)}]),
            mnesia:transform_table(
                offline_msg,
                fun({_, U, TS, F, T, P}) ->
                        {xmlelement, _Name, _Attrs, Els} = P,
                        Expire = mod_offline:find_x_expire(TS, Els),
                        #offline_msg{us = U,
                            timestamp = TS,
                            expire = Expire,
                            from = F,
                            to = T,
                            packet = P}
                end, Fields),
            F1 = fun() ->
                    mnesia:write_lock_table(mod_offline_tmp_table),
                    mnesia:foldl(
                        fun(#offline_msg{us = U} = R, _) ->
                                mnesia:dirty_write(
                                    mod_offline_tmp_table,
                                    R#offline_msg{us = {U, Host}})
                        end, ok, offline_msg)
            end,
            mnesia:transaction(F1),
            mnesia:clear_table(offline_msg),
            F2 = fun() ->
                    mnesia:write_lock_table(offline_msg),
                    mnesia:foldl(
                        fun(R, _) ->
                                mnesia:dirty_write(R)
                        end, ok, mod_offline_tmp_table)
            end,
            mnesia:transaction(F2),
            mnesia:delete_table(mod_offline_tmp_table);
        _ ->
            ?INFO_MSG("Recreating offline_msg table", []),
            mnesia:transform_table(offline_msg, ignore, Fields)
    end.
