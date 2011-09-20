-module(offline_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(VERIFICATION_REPEATS, 20).
-define(SENDER_RUNS, 80).
-define(RECEIVER_RUNS, ?SENDER_RUNS * 4).
-define(SLEEPINESS, 20).

all() ->
    [{group, presence}, %% Verifies whether negative presence priority
                        %% stops a resource from receiving messages,
                        %% i.e. are they stored offline/discarded.

     {group, leakage},  %% Verify whether heavy load may cause riak backend
                        %% to corrupt data, i.e. lose or duplicate messages
                        %% which ought to bo stored offline.

     {group, load}      %% Load some data onto the server and halt.
    ].

groups() ->
    [{presence, [{repeat_until_any_ok, 5}], [
        negative_presence_no_mod_offline,
        negative_presence
        ]},

     {load, [sequence], [
        load_data
        ]},

     {leakage_parallel, [parallel], [
        leakage_sender,
        leakage_receiver
        ]},
     {leakage, [sequence, {repeat, ?VERIFICATION_REPEATS}], [
        {group, leakage_parallel},
        verify_none_leaked
        ]}].

suite() ->
    random:seed(now()),
    %dbg:tracer(),
    %dbg:p(all,call),
    %dbg:tpl(escalus_story, story, x),
    %dbg:tpl(escalus_story, start_clients, x),
    %dbg:tpl(escalus_story, drop_initial_stanzas, x),
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(leakage, Config) ->
    ets_owner_start(leakage),
    lists:foreach(fun(E) -> ets:insert(leakage, E) end,
        [{sent, 0}, {received_online, 0}, {received_offline, 0}]),
    escalus:create_users(Config);
init_per_group(leakage_parallel, Config) ->
    %% Rationale: group 'leakage' already created the users and this
    %% (leakage_parallel) group is encapsulated by 'leakage'.
    %% Same goes for end_per_group, see _1_.
    %% This can't be repeated as it will cause server side errors
    %% (i.e. creating users that already exist).
    Config;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(leakage, Config) ->
    leakage ! stop,
    escalus:delete_users(Config);
end_per_group(leakage_parallel, Config) ->
    %% _1_: just as in init_per_group, these users will be deleted
    %% in end_per_group(leakage, Config), so don't call escalus:delete_users
    Config;
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(negative_presence_no_mod_offline = CaseName, Config) ->
    escalus_ejabberd:rpc(mod_offline, clear_table, []),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(negative_presence = CaseName, Config) ->
    escalus_ejabberd:rpc(mod_offline, clear_table, []),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(leakage_sender = CaseName, Config) ->
    escalus_ejabberd:rpc(mod_offline, clear_table, []),
    escalus:init_per_testcase(CaseName,
        [fake_client(Config, jane) | Config]);
init_per_testcase(leakage_receiver = CaseName, Config) ->
    escalus_ejabberd:rpc(mod_offline, clear_table, []),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

negative_presence_no_mod_offline(Config) ->
    escalus:story(Config, [1, 1], fun(Mary, Jane) ->

        Msg = "Hi, Jane!",
        BareJane = bare_jid(Jane),
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),
        escalus_assert:is_chat_message(Msg,
            escalus_client:wait_for_stanza(Jane)),

        set_negative_presence(Jane),

        %% chat again
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),

        %% Mary should receive service-unavailable
        Error = escalus_client:wait_for_stanza(Mary),
        escalus_utils:log_stanzas("Should be service-unavailable", [Error]),
        escalus_assert:is_error(Error, <<"cancel">>,
            'service-unavailable'),

        %% Jane should not receive message
        timer:sleep(500),
        escalus_assert:has_no_stanzas(Jane)

        end).

negative_presence(Config) ->
    escalus:story(Config, [1, 1], fun(Mary, Jane) ->

        Msg = "Hi, Jane!",
        BareJane = bare_jid(Jane),
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),
        escalus_assert:is_chat_message(Msg,
            escalus_client:wait_for_stanza(Jane)),

        set_negative_presence(Jane),

        %% Jane should not receive message
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),
        timer:sleep(500),
        escalus_assert:has_no_stanzas(Jane),

        %% set non-negative presence
        timer:sleep(500),
        set_available(Jane),

        %% Mary should not receive anything
        escalus_assert:has_no_stanzas(Mary),

        %% Jane should receive delayed message
        DelayedMessage = escalus_client:wait_for_stanza(Jane),
        %escalus_utils:log_stanzas("Delayed message", [DelayedMessage]),
        escalus_assert:is_chat_message(Msg, DelayedMessage),
        true = is_delayed(DelayedMessage)

        end).

load_data(Config) ->
    escalus:story(Config, [1, 1], fun(Mary, Jane) ->

        UselessPresence1 = escalus_client:wait_for_stanza(Jane),
        escalus_utils:log_stanzas("Should be presence", [UselessPresence1]),
        UselessPresence2 = escalus_client:wait_for_stanza(Mary),
        escalus_utils:log_stanzas("Should be presence", [UselessPresence2]),

        Msg = "Hi, Jane!",
        BareJane = bare_jid(Jane),
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),
        escalus_assert:is_chat_message(Msg,
            escalus_client:wait_for_stanza(Jane)),

        set_negative_presence(Jane),

        %% Jane should not receive message
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),
        erlang:halt()

        end).

leakage_sender() -> [{require, {escalus_users, mary}},
                     {require, {escalus_users, jane}}].

leakage_sender(Config) ->
    escalus:story(Config, [{mary,1}], fun(Mary) ->

        Jane = ?config(jane, Config),
        %% ensure receiver has time to start first,
        %% as arriving messages crash escalus:story
        ?t:sleep(500),
        sender_loop(Mary, Jane, ?SENDER_RUNS)

        end).

leakage_receiver() -> [{require, {escalus_users, jane}}].

leakage_receiver(Config) ->
    escalus:story(Config, [{jane,1}], fun(Jane) ->

        receiver_loop(Jane, ?RECEIVER_RUNS)

        end).

verify_none_leaked(_Config) ->
    Stats = [Sent, ReceivedOffline, ReceivedOnline] =
        [ ets:lookup_element(leakage, Key, 2) ||
          Key <- [sent, received_offline, received_online] ],
    error_logger:info_msg(
        "sent: ~p~nreceived_offline: ~p~nreceived_online: ~p~n", Stats),
    Sent = ReceivedOnline + ReceivedOffline.

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

sender_loop(_Client, _Addressee, 0) ->
    ok;
sender_loop(Client, Addressee, SendN) ->
    %?t:sleep(?SLEEPINESS + random:uniform(?SLEEPINESS)),
    escalus_client:send(Client,
        escalus_stanza:chat_to(Addressee, "Hi!")),
    ets:update_counter(leakage, sent, 1),
    sender_loop(Client, Addressee, SendN - 1).

receiver_loop(_Client, 0) ->
    ok;
receiver_loop(Client, WaitN) ->
    set_negative_presence_async(Client),
    ?t:sleep(?SLEEPINESS + random:uniform(?SLEEPINESS)),
    case escalus_client:has_stanzas(Client) of
        true ->
            Stanza = escalus_client:wait_for_stanza(Client),
            %escalus_utils:log_stanzas("Received stanza", [Stanza]),
            case {is_message(Stanza), is_delayed(Stanza)} of
                {true, true} ->
                    ets:update_counter(leakage, received_offline, 1);
                {true, false} ->
                    ets:update_counter(leakage, received_online, 1);
                _ ->
                    ok
            end;
        false ->
            ok
    end,
    set_available_async(Client),
    ?t:sleep(?SLEEPINESS + random:uniform(?SLEEPINESS)),
    receiver_loop(Client, WaitN - 1).

is_message(Stanza) ->
    'message' == exmpp_xml:get_name_as_atom(Stanza).

ets_owner_start(EtsName) ->
    register(EtsName, spawn(?MODULE, ets_owner_init, [self(), EtsName])),
    receive started -> ok end.

ets_owner_init(Parent, EtsName) ->
    EtsName = ets:new(EtsName, [named_table, public]),
    Parent ! started,
    ets_owner_loop(EtsName).

ets_owner_loop(EtsName) ->
    receive
        stop ->
            ets:delete(EtsName);
        _ ->
            ets_owner_loop(EtsName)
    end.

fake_client(Config, Name) ->
    Specs = ?config(escalus_users, Config),
    UserSpec = proplists:get_value(Name, Specs),
    [User, Server] =
        [ proplists:get_value(Key, UserSpec) || Key <- [username, server] ],
    {Name, #client{jid = User ++ "@" ++ Server}}.

set_negative_presence_async(Client) ->
    NegativePresence = prioritized_presence(available, -1),
    escalus_client:send(Client, NegativePresence)
    %, escalus_utils:log_stanzas("Negative presence", [NegativePresence])
    .

set_negative_presence(Client) ->
    set_negative_presence_async(Client),
    escalus_assert:is_presence_stanza(
        escalus_client:wait_for_stanza(Client)).

set_available_async(Client) ->
    escalus_client:send(Client, escalus_stanza:presence(available)).

set_available(Client) ->
    set_available_async(Client),
    escalus_assert:is_presence_stanza(
        escalus_client:wait_for_stanza(Client)).

bare_jid(Client) ->
    hd(string:tokens(binary_to_list(Client#client.jid), "/")).

prioritized_presence(Type, Priority) ->
    exmpp_presence:set_priority(
        escalus_stanza:presence(Type),
        Priority).

chat_to_jid(ClientJid, Msg) ->
    exmpp_stanza:set_recipient(
        exmpp_message:chat(Msg), 
        ClientJid).

is_delayed(Stanza) ->
    true == exmpp_xml:has_element(Stanza, 'delay').
